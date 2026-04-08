# -*- coding: utf-8 -*-
"""
Created July 10 2025 
Modified September 8 2025

@author: Greg Miller

This application provides a GUI for the NPZ comparison tool (compare_restart.py).
"""
import tkinter as tk
from tkinter import filedialog, messagebox, Listbox, Scrollbar
import io
import sys
import os
import itertools

try:
    from compare_restart import compare_npz_files, write_report
except ImportError:
    messagebox.showerror("Import Error", "Could not find restart.py for import." \
                         "\nPlease ensure it is in the same directory as this file.")
    sys.exit(1)
except Exception as e:
    messagebox.showerror("Import Error", f"Unexpected error occurred during import: {e}")
    sys.exit(1)

class ResultPopup(tk.Toplevel):
    """
    A Toplevel window for displaying comparison results and messages.

    This window is modal and features a scrollable text area for output.
    """
    def __init__(self, parent, output_text):
        """
        Initializes the ResultPopup window.

        Parameters
        ----------
        parent : tk.Tk or tk.Toplevel
            The parent window for this popup.
        output_text : str
            The text content to be displayed in the popup's text area.
        """
        super().__init__(parent)

        self.title("Comparison Results")
        self.geometry("600x400")
        self.transient(parent)
        self.grab_set()

        # add readonly text widget for results
        self.output_display = tk.Text(self, wrap="word", state="normal")
        self.output_display.insert(tk.END, output_text)
        self.output_display.configure(state="disabled")

        # scrollbar
        scrollbar = tk.Scrollbar(self, command=self.output_display.yview)
        self.output_display.configure(yscrollcommand=scrollbar.set)

        # pack it in
        self.output_display.pack(side="left", fill="both", expand=True, padx=10, pady=10)
        scrollbar.pack(side="right", fill="y")

        # allow resizing
        self.grid_rowconfigure(0, weight=1)
        self.grid_columnconfigure(0, weight=1)

        # configure the 'x'
        self.protocol("WM_DELETE_WINDOW", self._on_closing)

    def _on_closing(self):
        """
        Handles the window closing event.

        Releases the grab and destroys the window.
        """
        self.grab_release()
        self.destroy()

class NpzCompareApp:
    """
    Main application class for the NPZ file comparison GUI.

    Manages user input, calls the comparison logic from 'compare_restart.py',
    and displays results in a popup window.
    """
    def __init__(self, main):
        """
        Initializes the main application window and its widgets.

        Parameters
        ----------
        main : tk.Tk
            The root Tkinter window.
        """
        self.main = main
        main.title("NEMS NPZ Comparison Tool")

        # List to hold paths of selected NPZ files
        self.npz_files = []

        # Vars for other shared data
        self.tolerance = tk.DoubleVar(value=0.0001)
        self.output_folder = tk.StringVar() # Now a folder, not a base filename
        self.overwrite_output_var = tk.BooleanVar(value=False)
        self.detailed_output_var = tk.BooleanVar(value=False) # A switch for detailed output
        self.vartext = tk.StringVar()

        self.script_dir = os.path.dirname(os.path.abspath(__file__))
        self.last_dir = self.script_dir

        # Frame for file selection and listbox
        file_selection_frame = tk.Frame(main, padx=10, pady=10)
        file_selection_frame.pack(pady=10, fill='both', expand=True)
        tk.Label(file_selection_frame, text="Selected NPZ Files:").pack(anchor='w', pady=(0, 5))

        # Listbox to display NPZ files
        self.file_listbox = Listbox(file_selection_frame,
                                    selectmode=tk.EXTENDED,
                                    width=80, height=10)
        self.file_listbox.pack(side="left", fill="both", expand=True)

        # Scrollbar for the listbox
        list_scrollbar = Scrollbar(file_selection_frame,
                                   orient="vertical",
                                   command=self.file_listbox.yview)
        list_scrollbar.pack(side="right", fill="y")
        self.file_listbox.config(yscrollcommand=list_scrollbar.set)

        # Frame for file management buttons
        file_buttons_frame = tk.Frame(main)
        file_buttons_frame.pack(pady=(0, 10))

        self.add_file_button = tk.Button(file_buttons_frame, 
                                         text="Add NPZ File...", 
                                         command=self.add_npz_file)
        self.add_file_button.pack(side="left", padx=5)

        self.remove_file_button = tk.Button(file_buttons_frame, 
                                            text="Remove Selected File", 
                                            command=self.remove_npz_file)
        self.remove_file_button.pack(side="left", padx=5)

        # Tolerance and Report Output Folder section
        options_frame = tk.Frame(main, padx=10, pady=10)
        options_frame.pack(pady=10, fill='x')

        tk.Label(options_frame,
                 text="Tolerance").grid(row=0, column=0, sticky="w", pady=5)
        self.tolerance_entry = tk.Entry(options_frame, textvariable=self.tolerance, width=15)
        self.tolerance_entry.grid(row=0, column=1, sticky="w", padx=5, pady=5)

        # Output file selector
        tk.Label(options_frame,
                 text="Report Output Folder:").grid(row=1, column=0, sticky='w', pady=5)
        self.output_folder_entry = tk.Entry(options_frame,
                                            textvariable=self.output_folder,
                                            width=50)
        self.output_folder_entry.grid(row=1, column=1, padx=5, pady=5)
        self.output_folder_button = tk.Button(options_frame, text="Browse...",
                                       command=self.select_output_folder)
        self.output_folder_button.grid(row=1, column=2, padx=5, pady=5)
        
        # Variable Input
        tk.Label(options_frame,
                 text="Variables for Detailed Comparison: (ex. AB32 or AB32/ab_allbank_avl) separated by comma:").grid(row=2, column=0, columnspan=2, sticky="w", pady=5)
        self.variable_entry = tk.Entry(options_frame, textvariable=self.vartext, width=90)
        self.variable_entry.grid(row=3, column=0, columnspan=3, sticky="w", padx=5, pady=5)

        # Overwrite checkbox and Compare button
        btn_frame = tk.Frame(main)
        btn_frame.pack(fill='x', padx=10, pady=10)

        self.overwrite_checkbox = tk.Checkbutton(btn_frame, text="Overwrite Output File(s)",
                                                 variable=self.overwrite_output_var)
        self.overwrite_checkbox.pack(side="left", padx=10)
        
        # Detailed checkbox
        self.detailed_checkbox = tk.Checkbutton(btn_frame, text="Generate Detailed Comparison",
                                                 variable=self.detailed_output_var)
        self.detailed_checkbox.pack(side="left", padx=5)

        self.compare_button = tk.Button(btn_frame, 
                                        text="Compare Files",
                                        command=self.run_comparison)
        self.compare_button.pack(side="right", padx=5)

    def add_files_to_list(self, file_paths):
        """Add a list of files to the internal list then update the listbox.
        Handles duplicates and ensures only files with .NPZ extension are allowed.
        
        Parameters
        ----------
        file_paths  :   list or tuple
            A list or tuple of file paths to add to the app.
        """
        added_count = 0
        for f in file_paths:
            normalized_path = os.path.normpath(f)
            if normalized_path.lower().endswith("npz") and normalized_path not in self.npz_files:
                self.npz_files.append(normalized_path)
                added_count += 1
            elif normalized_path in self.npz_files:
                print(f"Skipping duplicate file: {os.path.basename(normalized_path)}")
            else:
                print(f"Skipping file with bad extension: {os.path.basename(normalized_path)}")
        if added_count > 0:
            self.npz_files.sort()
            self.update_file_listbox()
        elif added_count == 0 and file_paths:
            messagebox.showinfo("No new files were added during drag-and-drop.\n"\
                                "All files were duplicates or not NPZ files.")

    def add_npz_file(self):
        """
        Opens a file selection dialog and adds selected NPZ file to the list.
        """
        file_paths = filedialog.askopenfilenames(
            title="Select NPZ File(s)",
            initialdir=self.last_dir,
            filetypes=[("NPZ files", "*.npz"), ("All files", "*.*")]
        )
        if file_paths:
            self.add_files_to_list(file_paths)
            self.last_dir = os.path.dirname(file_paths[0])

    def remove_npz_file(self):
        """
        Removes the selected NPZ file from the listbox and internal list.
        """
        selected_indices = list(self.file_listbox.curselection())
        if not selected_indices:
            messagebox.showinfo("No files selected.", "Please select one or more files to remove.")
            return

        files_to_remove = []
        for i in selected_indices:
            files_to_remove.append(self.file_listbox.get(i))

        for f in files_to_remove:
            if f in self.npz_files:
                self.npz_files.remove(f)
        self.update_file_listbox()

    def update_file_listbox(self):
        """
        Clears and repopulates the listbox with current NPZ files.
        """
        self.file_listbox.delete(0, tk.END)
        for f_path in self.npz_files:
            self.file_listbox.insert(tk.END, f_path)

    def select_output_folder(self):
        """
        Opens a directory selection dialog for the report output folder.
        """
        folder_path = filedialog.askdirectory(
            title="Choose Output Folder for Reports",
            initialdir=self.last_dir
        )
        if folder_path:
            self.output_folder.set(folder_path)
            self.last_dir = folder_path

    def _get_npz_file_identifier(self, file_path):
        """
        Extracts a unique, descriptive identifier from the NPZ file path.
        For paths like Q:/output/gnm/r26_lfmmOFF/d071125e/restart.npz,
        it should return 'd071125e-restart'.
        If 'restart.npz' is present, it uses the parent directory name
        followed by '-restart'. Otherwise, it uses the full filename without extension.
        """
        base_name = os.path.basename(file_path)
        if base_name.lower() == "restart.npz":
            # Get the parent directory name, which should be 'd071125e' in the example
            parent_dir_name = os.path.basename(os.path.dirname(file_path))
            return f"{parent_dir_name}-{os.path.splitext(base_name)[0]}"
        else:
            # For other filenames, just use the filename without extension
            return os.path.splitext(base_name)[0]

    def run_comparison(self):
        """
        Executes the comparison process for all unique pairs of selected files.
        """
        if len(self.npz_files) < 2:
            messagebox.showerror("Error", "Please select at least two NPZ files for comparison.")
            return

        output_folder = self.output_folder.get()
        if not output_folder:
            messagebox.showerror("Error", "Please select an output folder for reports.")
            return

        if not os.path.isdir(output_folder):
            messagebox.showerror("Error",
                f"The selected output folder '{output_folder}'"\
                 " does not exist or is not a directory. Please choose a valid folder.")
            return

        try:
            tolerance_value = self.tolerance.get()
            if not isinstance(tolerance_value, (float, int)) or tolerance_value < 0:
                raise ValueError("Tolerance must be a non-negative number.")
        except (tk.TclError, ValueError) as e:
            messagebox.showerror("Input Error", f"Invalid tolerance value: {e}")
            return

        # capture stdout for later readback
        all_output_messages = []
        old_stdout = sys.stdout
        redirected_output_buffer = io.StringIO()
        sys.stdout = redirected_output_buffer

        comparison_results_summary = []

        try:
            # Generate all unique pairs
            file_pairs = list(itertools.combinations(self.npz_files, 2))

            for i, (file1_path, file2_path) in enumerate(file_pairs):
                # Construct unique report file name for each pair
                id1 = self._get_npz_file_identifier(file1_path)
                id2 = self._get_npz_file_identifier(file2_path)

                # Ensure the report filename is valid for the file system
                report_filename = f"{id1}-vs-{id2}.csv".replace(':', '_').replace('/', '_').replace('\\', '_')
                current_report_path = os.path.join(output_folder, report_filename)

                comparison_results_summary.append(f"\n--- Comparing: {os.path.basename(file1_path)} vs {os.path.basename(file2_path)} ---")
                comparison_results_summary.append(f"Report will be saved to: {current_report_path}")

                # Check if report file exists and if overwrite is allowed
                if os.path.exists(current_report_path) and not self.overwrite_output_var.get():
                    comparison_results_summary.append(f"Skipping comparison {id1} vs {id2}: Report file '{current_report_path}' already exists and overwrite is disabled.")
                    continue # Skip this pair

                try:
                    if self.detailed_output_var.get() == True:
                        if self.vartext.get().strip() == "":
                            variables = []
                            print("No variables entered.")
                        else:
                            variables = [var.strip() for var in self.vartext.get().split(',')]
                            print("You entered the following variables:", variables)
                    else:
                        variables = []
                    problem_keys, array_cords, calc_string, diff_value, core_messages = compare_npz_files(file1_path, file2_path, tolerance_value, variables)
                    comparison_results_summary.extend(core_messages)

                    if not problem_keys:
                        comparison_results_summary.append(f"No differences found between {id1} and {id2}.")
                        with open(current_report_path, 'w', encoding="UTF8") as f:
                            f.write("No differences found.")
                    else:
                        write_report(problem_keys, array_cords, calc_string, diff_value, current_report_path, self.detailed_output_var.get())
                        comparison_results_summary.append(f"Differences found between {id1} and {id2}! Details in {current_report_path}")

                        try:
                            with open(current_report_path, 'r', encoding="UTF8") as f:
                                csv_content = f.read()
                                comparison_results_summary.append(f"\n--- Report Content for {id1} vs {id2} ---\n" + csv_content)
                        except Exception as e:
                            comparison_results_summary.append(f"\nFailed to read report file '{current_report_path}': {e}")
                except Exception as e:
                    comparison_results_summary.append(f"Error comparing {id1} vs {id2}: {e}")

            comparison_results_summary.append("\nAll comparisons complete.")

        except ValueError as e:
            all_output_messages.append(f"Error during comparison setup: {e}")
            messagebox.showerror("Comparison Setup Error", str(e))
        except Exception as e:
            all_output_messages.append(f"Unhandled exception occurred: {e}\n Please check the console.")
            import traceback
            traceback.print_exc()
            messagebox.showerror("Unexpected Error", f"Unexpected error: {e}")
        finally:
            sys.stdout = old_stdout # Restore stdout

        # Combine all output and display it
        final_output_text = "\n".join(comparison_results_summary)
        captured_script_output = redirected_output_buffer.getvalue().strip()
        if captured_script_output:
            final_output_text += "\n\n--- Script Internal Output from all comparisons ---\n" + captured_script_output
        ResultPopup(self.main, final_output_text)

if __name__ == "__main__":
    root = tk.Tk()
    app = NpzCompareApp(root)
    root.mainloop()
