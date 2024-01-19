from DataModel.Model import Model
import pytest

class StatusHelper:
    status_list = ['undefined', 'skip', 'active', 'inactive']
    
    def __init__(self):
        pass
        
    def get_status(self, test_name):    
        dict_status = Model.getInstance().files.status_control.dict_status_list
        #dict_status = model.files.status_control.dict_status_list
        
        #test_name=self.test_name
               
        if not dict_status.get(test_name):
            return self.status_list[0]
        elif dict_status.get(test_name) == self.status_list[1]:
            return self.status_list[1]
        else:
            return dict_status.get(test_name)        
    
    def skip_test(self, status):
        if status == self.status_list[0]:
            pytest.skip("The test case is undefined in the validator_controller.csv.")
        elif status == self.status_list[1]:
            pytest.skip("The test case is marked as skipped.")