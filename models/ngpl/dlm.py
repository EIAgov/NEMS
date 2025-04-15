import numpy as np
import pandas as pd
import sys 
from copy import deepcopy
from filterpy.kalman import KalmanFilter

"""
A Dynamic Linear Model (DLM) class for time series analysis and forecasting.

This class implements a DLM, modeled after R's dlm library, suitable for statistical analysis
and machine learning applications. It incorporates a Kalman Filter for state estimation and
forecasting.

Attributes:
    components (dict): A dictionary containing the model components.
    kf (KalmanFilter): An instance of the KalmanFilter class for state estimation.

Raises:
    ValueError: If any required component is missing or if there are dimension mismatches.
    TypeError: If any component is not numeric.

Parameters:
    **kwargs: Variable length keyword arguments. Expected components are:
        - m0 (np.array): Initial state estimate.
        - C0 (np.array): Initial covariance estimate.
        - FF (np.array): Observation matrix.
        - V (np.array): Observation noise covariance.
        - GG (np.array): Transition matrix.
        - W (np.array): Process noise covariance.
        Additional time-varying components can be provided (JFF, JV, JGG, JW, X).
"""
class dlm:

    def __init__(self, **kwargs):
        # Unpacking keyword arguments into a dictionary
        self.components = kwargs
        
        # Required components for the DLM model
        required_components = ["m0", "C0", "FF", "V", "GG", "W"]
        
        # Check if any required component is missing
        for comp in required_components:
            if comp not in self.components:
                raise ValueError(f"Component(s) {comp} is (are) missing")
        
        # Ensure that the matrices are numpy arrays and validate numeric requirement
        for comp in required_components[1:]:
            self.components[comp] = np.asmatrix(self.components.get(comp))
            if not np.issubdtype(self.components[comp].dtype, np.number):
                raise TypeError(f"Component {comp} must be numeric")
        
        # Dimensions of FF
        m, p = self.components['FF'].shape

        # Validate the dimensions of the matrices
        self._validate_dimensions(m, p)
        
        # Check for time-varying components
        self._check_time_varying_components(m, p)

        # Initialize the Kalman Filter
        self.kf = KalmanFilter(dim_x=self.components['GG'].shape[1], 
                               dim_z=self.components['FF'].shape[0])

        # Set the initial state
        self.kf.x = self.components['m0']

        # Set the initial state covariance
        self.kf.P = self.components['C0']

        # Set the transition matrix
        self.kf.F = self.components['GG']

        # Set the observation matrix
        self.kf.H = self.components['FF']

        # Set the process noise covariance
        self.kf.Q = self.components['W']

        # Set the observation noise covariance
        self.kf.R = self.components['V']
    
    def _validate_dimensions(self, m, p):
        """
        Validates the dimensions of the matrices in the model components.

        Parameters:
            m (int): The number of rows in the observation matrix FF.
            p (int): The number of columns in the observation matrix FF.

        Raises:
            ValueError: If there are any dimension mismatches among the matrices.
        """
        ...
        # Validate dimensions and variance matrix properties
        if not (self.components['V'].shape == (m, m)):
            raise ValueError("Incompatible dimensions of matrices V")
        if not (self.components['GG'].shape == (p, p)):
            raise ValueError("Incompatible dimensions of matrices GG")
        if not (self.components['W'].shape == (p, p)):
            raise ValueError("Incompatible dimensions of matrices W")
        if not (self.components['C0'].shape == (p, p)):
            raise ValueError("Incompatible dimensions of matrices C0")
        if not (self.components['m0'].shape == (p, 1)):
            raise ValueError("m0 must be a numeric vector of length equal to ncol of component FF")
        if not self._is_valid_variance_matrix(self.components['C0']):
            raise ValueError("C0 is not a valid variance matrix")
    
    def _is_valid_variance_matrix(self, matrix):
        """
        Checks if a given matrix is a valid variance matrix (symmetric and non-negative eigenvalues).

        Parameters:
            matrix (np.array): The matrix to be checked.

        Returns:
            bool: True if the matrix is a valid variance matrix, False otherwise.
        """
        return np.allclose(matrix, matrix.T) and np.all(np.linalg.eigvals(matrix) >= 0)

    def _check_time_varying_components(self, m, p):
        """
        Checks and validates the time-varying components of the model, if any.

        Parameters:
            m (int): The number of rows in the observation matrix FF.
            p (int): The number of columns in the observation matrix FF.

        Raises:
            ValueError: If there are any issues with the dimensions or presence of time-varying components.
        """
        # Check for time-varying components
        time_varying_components = ["JFF", "JV", "JGG", "JW"]
        for comp in time_varying_components:
            if comp in self.components:
                self.components[comp] = np.asmatrix(self.components.get(comp))
                if comp == 'JFF' and not (self.components['JFF'].shape[0] == m):
                    raise ValueError("Invalid component JFF: row dimension does not match")
                elif comp == 'JV' and not (self.components['JV'].shape == (m, m)):
                    raise ValueError("Invalid component JV: dimensions do not match")
                elif comp == 'JGG' and not (self.components['JGG'].shape == (p, p)):
                    raise ValueError("Invalid component JGG: dimensions do not match")
                elif comp == 'JW' and not (self.components['JW'].shape == (p, p)):
                    raise ValueError("Invalid component JW: dimensions do not match")

        # Validate the 'X' component if time-varying components exist
        if any(comp in self.components for comp in time_varying_components):
            if 'X' not in self.components:
                raise ValueError("Component X must be provided for time-varying models")
            self.components['X'] = np.asmatrix(self.components['X'])
            if not np.issubdtype(self.components['X'].dtype, np.number):
                raise ValueError("Component X must be numeric")

    def filter(self, observations):
        """
        Applies the Kalman filter to the provided observations.

        Parameters:
            observations (list or np.ndarray): The observations to be filtered.

        Returns:
            dict: A dictionary containing filtered states ('m') and forecasts ('f').
        """
        filtered_states = []
        forecasts = []  

        for t, obs in enumerate(observations):
            # print(t)
            # Predict the next state
            self.kf.predict()

            # Reshape the observation to meet filterpy's requirements (dim_z, 1)
            obs_reshaped = np.reshape(obs, (self.kf.dim_z, 1))

            self.kf.update(obs_reshaped)

            filtered_states.append(self.kf.x.copy())

            # One-step forecast using the updated state and the transition matrix
            next_state = self.components['GG'] @ self.kf.x
            forecast = self.components['JFF'] @ next_state
            #if t>=39:
            #    import pdb; pdb.set_trace()
            # Reshape or truncate the forecast to match the dimensions of your observations (40x2)
            forecast = forecast[:len(observations), :2]  # Adjust this based on your model specifics

            forecasts.append(forecast)
            # If FF is time-varying, update it before the next predict step
            if 'JFF' in self.components:
                self.update_FF(self.components['JFF'], self.components['X'], t)
        
        forecasts = np.array(forecasts).reshape(-1, 2)  # Ensuring the shape is 40x2
        return {'m': np.array(filtered_states), 'f': forecasts}

    def update_FF(self, JFF, X, t):
        """
        Updates the observation matrix FF based on the JFF matrix and the X matrix at time t.

        Parameters:
            JFF (np.array): The matrix indicating updates to FF.
            X (np.array): The matrix containing the time-varying components.
            t (int): The current time step.
        """
        # JFF is an array where non-zero elements indicate updates to FF
        nz_indices = np.nonzero(JFF)

        for idx in range(len(nz_indices[0])):
            i, j = nz_indices[0][idx], nz_indices[1][idx]
            # Convert value_index to integer if JFF contains floating-point numbers.
            # JFF uses one-based indexing for the columns of X.
            value_index = int(JFF[i, j]) - 1
            self.kf.H[i, j] = X[t, value_index]
