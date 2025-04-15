# -*- coding: utf-8 -*-
"""
Created on Apr 25 2023

@author: Claire Su
"""
import os, subprocess, csv
from tkinter import Tk, Label, Listbox, LabelFrame, Button, Entry, StringVar, IntVar, Radiobutton, Checkbutton
from tkinter import N, E, S, W, NE, NW, SE, SW
from tkinter import ttk
from tkinter import filedialog, messagebox
from scripts.setup.src.network_drive import NetworkDrive

class NEMSApp():
    def __init__(self, baseout, drive_list):
        '''
        The init method to layout the UI and bind the button actions.

        Parameters
        ----------
        baseout : string
            the default base output directory path when the GUI launches.
        drive_list : list
            the list of drive info displayed on the drive space list box.

        Returns
        -------
        None.

        '''
        self.user = os.environ['USERNAME'].lower()
        self.root_script_dir = os.path.join(os.getcwd(),'scripts')
        self.root_scedes_dir = os.path.join(os.getcwd(),'scedes')
        self.scedes = (os.path.join(self.root_scedes_dir,'scedes.ref2025')).replace('\\', '/')
        self.outdir = baseout
        self.drive_list = drive_list
        self.NEMSPYENV = os.environ['NEMSPYENV']
        
        # beautify the UI look with a consistent width of an entry and background color
        self.entry_width = '50'
        self.bg = r'#88cffa'
        
        # read in the UI element settings:
        self.local_radio_button_status, self.queue_radio_button_status = self.get_ui_element_settings()

        # create window
        self.root = Tk()
        self.root.title("NEMS: National Energy Modeling System")
        self.root.config(bg=self.bg)

        # add drive list
        self.lbl_drive = Label(self.root, text="Available Drive Space:", background=self.bg)
        self.lbl_drive.grid(column=0, row=1, padx=5, pady=0, sticky=(N))
        self.ls_drive = Listbox(self.root)
        self.ls_drive.grid(column=0, row=2, padx=5, pady=0, sticky=(N,S))
        self.ls_drive.bind('<<ListboxSelect>>', self.populate_selected_drive)
        self.insert_drive_space_list()
        
        # 1)add Select frame
        self.notebook = ttk.Notebook(self.root, padding="5 5 5 5")
        self.notebook.grid(column=2, row=2, sticky=(N,W,E,S))

        self.frm_main = ttk.Frame(self.notebook)
        self.notebook.add(self.frm_main, text="Start Run")

        self.grp_select = LabelFrame(self.frm_main, text="Select:", padx=5, pady=5)
        self.grp_select.grid(column=0, row=0, padx=5, pady=5, sticky=(W,E), columnspan = 2)
        
        # 1.1)choose scedes
        self.btn_scedes = Button(self.grp_select, text="scedes", width="5", command=self.choose_scedes)
        self.btn_scedes.grid(column=0, row=0, padx=5, pady=5, sticky=(W))
        self.lbl_scedes = Entry(self.grp_select, width=self.entry_width)
        self.lbl_scedes.insert(0, self.scedes)
        self.lbl_scedes.grid(column=1, row=0, padx=5, pady=5, sticky=(W))
        
        # 1.2)choose outdir
        self.btn_outdir = Button(self.grp_select, text="outdir", width="5", command=lambda:self.choose_outdir(self.outdir))
        self.btn_outdir.grid(column=0, row=1, padx=5, pady=5, sticky=(W))
        self.lbl_outdir = Entry(self.grp_select, width=self.entry_width)
        self.lbl_outdir.insert(0, self.outdir)
        self.lbl_outdir.grid(column=1, row=1, padx=5, pady=5, sticky=(W))
        
        # 2)add Mode frame
        self.grp_mode = LabelFrame(self.frm_main, text="Mode:", padx=5, pady=5)
        self.grp_mode.grid(column=0, row=2, padx=5, pady=5, sticky=(W,E))

        self.var=StringVar(None,'jog')
        self.radio_jog = Radiobutton(self.grp_mode, text="jognems", variable=self.var, value='jog')
        self.radio_par = Radiobutton(self.grp_mode, text="parnems", variable=self.var, value='par')
        #self.radio_pre = Radiobutton(self.grp_mode, text="EMM preprocessor", variable=self.var, value='pre')

        self.radio_jog.grid(column=0, row=0, padx=5, pady=0, sticky=(W))
        self.radio_par.grid(column=0, row=1, padx=5, pady=0, sticky=(W))
        #self.radio_pre.grid(column=0, row=2, padx=5, pady=0, sticky=(W))        

        # 3)add Job frame
        self.grp_job = LabelFrame(self.frm_main, text="Job Type:", padx=5, pady=5)
        self.grp_job.grid(column=1, row=2, padx=5, pady=5, sticky=(W,E))

        if self.local_radio_button_status == 'normal':
            self.var_job=StringVar(None,'local')
        else:
            self.var_job=StringVar(None,'queue')
        self.radio_local = Radiobutton(self.grp_job, text="local", variable=self.var_job, value='local')
        self.radio_queue = Radiobutton(self.grp_job, text="queue", variable=self.var_job, value='queue')
        self.radio_local.config(state=self.local_radio_button_status)
        self.radio_queue.config(state=self.queue_radio_button_status)

        self.radio_local.grid(column=0, row=0, padx=5, pady=0, sticky=(W))
        self.radio_queue.grid(column=0, row=1, padx=5, pady=0, sticky=(W))

        # 4)add Run Button
        self.btn_run = Button(self.grp_job, text="Run", width="8", command=self.validate_args)
        self.btn_run.config(bg=self.bg, fg='white', font='sans 10 bold')
        self.btn_run.place(relx=.78, rely=.5, anchor="c")
        
        # 5)add Checkbox of onlu create run folder
        self.chk_btn_var = IntVar()
        self.chk_btn = Checkbutton(self.frm_main, text='Only create run folder',variable=self.chk_btn_var, onvalue=1, offvalue=0)
        self.chk_btn.grid(column=1, row=3, padx=5, pady=5, sticky=(W))

        # Stop Run tab
        self.frm_stop = ttk.Frame(self.notebook)
        self.notebook.add(self.frm_stop, text="Stop Run")

        # Stop Run LabelFrame
        self.grp_stop = LabelFrame(self.frm_stop, padx=5, pady=5)
        self.grp_stop.grid()
        self.grp_stop.grid(column=0, row=0, padx=5, pady=5)

        # Stop run Output Directory Label
        self.lbl_stopout = Label(
            self.grp_stop, text="Output Directory:", justify="left"
        )
        self.lbl_stopout.grid(column=0, row=1, padx=5, pady=0, stick=(W))

        # Stop run button
        self.ent_stop = Entry(self.grp_stop, width=int(self.entry_width) + 10)
        self.ent_stop.insert(0, r"D:\workdir\scenario\datekey")
        self.ent_stop.grid(column=0, row=2, padx=(7.5, 0), pady=(2.5, 7.5))
        self.ent_stop.bind("<Return>", self.stop_run)

        self.lbl_stop = Label(self.grp_stop)
        self.lbl_stop.grid(column=0, row=3, padx=10, pady=0, stick=(W))

        self.btn_stop = Button(
            self.grp_stop,
            text="Stop run",
            width="8",
            command=self.stop_run,
        )
        self.btn_stop.grid(column=0, row=3, sticky=(E))
        self.btn_stop.config(bg=self.bg, fg="white", font="sans 10 bold")

        self.root.mainloop()

    def get_ui_element_settings(self):
        '''
        Read the UI element configuration file nems_gui_settings.csv. Get and return the status of local and queue radio buttons.
        - the integration team developers can explicitly specify 'normal' (enable the radio button) in the configuration to enable both
          local and queue radio buttons.
        - when the audience are external public users, delete the configuration file and the following settings will be used:
          local_radio_button_status='normal' (the radio button will be enabled)
          queue_radio_button_status='disabled' (the radio button will be greyed out)
        - For EIA internal modelers, specify normal for queue and disabled for local in the configuration file.
          If local radio button setting is not specified, a 'disabled' status will be set.
          If queue radio button setting is not specified, a 'normal' status will be set.

        Returns
        -------
        local_radio_button_status : string
            the status setting ('normal' or 'disabled') of local radio button
        queue_radio_button_status : string
            the status setting ('normal' or 'disabled') of queue radio button.
        '''
        local_radio_button_status='normal'
        #queue_radio_button_status='disabled'
        queue_radio_button_status='normal'
        if not os.path.isfile('nems_gui_settings.csv'): return local_radio_button_status, queue_radio_button_status

        settings={}
        with open('nems_gui_settings.csv', 'r') as file:
            csvFile = csv.reader(file)
            for line in csvFile:
                settings[line[0]]=line[1]

        local_radio_button_status=settings.get('local_radio_button','disabled')
        queue_radio_button_status=settings.get('queue_radio_button','normal')
        return local_radio_button_status, queue_radio_button_status

    def insert_drive_space_list(self):
        '''
        Obtain a list of drive info and insert to the drive space list box.

        Returns
        -------
        ls : list
            the drive list to display on the list box.

        '''
        # get dynamic network drive list with free space info
        ls = self.drive_list
        
        for i in range(0,len(ls)):
            self.ls_drive.insert('end', ls[i])
        
        return ls

    def populate_selected_drive(self, event):
        '''
        Populate the user selected drive on the drive space list box to the output directory entry field.

        Parameters
        ----------
        event : event
            the list box selection event.

        Returns
        -------
        None.

        '''
        if not (self.ls_drive.curselection()):
            return
        
        selected = self.ls_drive.get(self.ls_drive.curselection())
        #if selected[:5].lower() == 'other': selected=''
        if selected[2]!='/':
            selected=selected[0:2]+f'/output/{self.user}'
        
        self.lbl_outdir.delete(0, 'end')
        self.lbl_outdir.insert(0, selected)
        
    def stop_run(self, event=None):
        """Create .txt files to stop a run, given the Output Directory

        Parameters
        ----------
            path:
                Output directory (ex. D:\workdir\ref2024\d020524)
        """
        self.lbl_stop.config(text="")
        path = os.path.abspath(self.ent_stop.get())  # abspath ensures formatting
        split = path.split("\\")  # Get list of directories
        paths = []
        pvals = ["p1", "p2", "p3"]
        outfile = "stop.txt"

        # Get p1, p2, p3 if given as part of input path
        pval = split.pop() if os.path.basename(path) in pvals else None
        key = split.pop()  # Datekey
        scen = split.pop()  # Scenairo

        # Check if run is on nem7, nem8, nem9, or nem10
        for num in range(7, 11):
            nem = os.path.abspath(f"\\\\nem{num}.eia.doe.gov\\workdir\\{scen}\\{key}")
            if os.path.isdir(nem):  # End loop if path exists
                paths.append(nem)
                break

        try:
            # Find p1, p2, and p3 folders
            for p in pvals:
                ppath = os.path.join(paths[0], p)
                if os.path.isdir(ppath):
                    paths.append(ppath)

            # Allow integration to stop any run regardless of user who initiated it
            integration = ["GOD", "dne", "rcs", "bto", "mc6", "jid", "ark", "djs", "jmw"]

            # Get user that launched run
            with open(os.path.join(paths[0], "launched.from"), mode="r") as file:
                launched = file.readlines()
            user = launched[0].split(" ")[-2]

            # Confirm that user is allowed to stop the run
            assert self.user in integration or self.user == user

            # Create blank files in datekey and p_ folders
            for path in paths:
                open(os.path.join(path, outfile), "a").close()
            self.lbl_stop.config(
                text=f"Run stopped successfully by {self.user} ({user} initiated run)",
                fg="green",
            )

        except (FileNotFoundError, IndexError):
            self.lbl_stop.config(text="Error: Check scenario and datekey", fg="red")

        except AssertionError:  # User tried to delete another's run
            self.lbl_stop.config(
                text=f"Error: {self.user} unable to delete {user}'s run", fg="red"
            )
    def choose_scedes(self):
        '''
        The action method of the scedes button, to open a file explorer dialog for user to select a scedes file.

        Returns
        -------
        None.

        '''
        self.lbl_scedes.delete(0, 'end')

        self.filename = filedialog.askopenfilename(initialdir=self.root_scedes_dir, 
                                                   title="NEMS: Select your scedes file",
                                                   filetypes=(("Scedes files","scedes.*"),("all files","*.*")))
        self.lbl_scedes.insert(0,self.filename)

    def choose_outdir(self, baseout=''):
        '''
        The action method of the outdir button, to open a file explorer dialog for user to select a directory.

        Parameters
        ----------
        baseout : string, optional
            the base output directory path when the file explorer dialog is open. The default is an empty string.

        Returns
        -------
        None.

        '''
        self.lbl_outdir.delete(0, 'end')
        
        self.filefolder = filedialog.askdirectory(initialdir=baseout, 
                                                  title="NEMS: Select your output directory")
        self.lbl_outdir.insert(0,self.filefolder)

    def run_selection(self):
        '''
        The method sends work to nems_submit.py. It gets invoked by validate_arg(), collects the info nems_submit.py requires, 
        generates a temporary runtime tmp_debug.bat and run it to send the request to nems_submit.py.

        Returns
        -------
        None.

        '''
        # parse the scedes file path to get the senario
        scenario_dir = os.path.dirname(self.lbl_scedes.get().strip()).replace('/','\\')
        filename,scenario = os.path.splitext(self.lbl_scedes.get().strip())
        scenario = scenario[1:]
        
        scpt_list = [f'cd {scenario_dir}']

        # for debug, clean up cmd settings
        #cmd_cleanup=r'start /i "%windir%\explorer.exe" "%windir%\system32\cmd.exe"'
        #scpt_list.append(f'{cmd_cleanup}\n')
        
        # EMM preprocessor needs the following 2 settings
        scpt_list.append(f'SET NEMS={os.getcwd()}'.replace('/','\\'))
        scpt_list.append(f'SET USER={self.user}')
        
        # I do not think we need to call the following commands.sh for alias but I leave it here for now
        #scpt_list.append(f'. {os.path.join(self.root_script_dir,"commands.sh")}'.replace('/','\\'))
        #scpt_list.append(f'{os.path.join(self.root_script_dir,"commands.sh")}'.replace('/','\\'))
        # if I have shell runs inside of cmd like the above line 215, my environmnet gets lost and has no printout interaction in that cmd prompt.
        
        file_debug = os.path.join(os.getcwd(),"tmp_debug.bat")
        file = os.path.join(self.root_script_dir,'setup','src','nems_submit.py')
        
        scpt_list.append(f"call {os.path.join(self.NEMSPYENV,'Scripts','activate')}")
        mode = self.var.get()
        job = self.var_job.get()
        chk_btn_var=str(self.chk_btn_var.get())
        if mode == 'pre':
            scpt_list.append(f'python {file} {mode}'.replace('/','\\'))
        else:
            scpt_list.append(f'python {file} {mode} {scenario} {self.lbl_outdir.get()} {job}{chk_btn_var}'.replace('/','\\'))
        # delete tis temporary runtime .bat file after run it. If you want to debug and keep the runtime .bat file, comment out the following line
        #scpt_list.append(r'(goto) 2>nul & del "%~f0"')
        with open(file_debug,'w+') as f:
            for i in range(0,len(scpt_list)):
                f.write(scpt_list[i]+'\n')
        
        subprocess.run('start tmp_debug.bat', shell=True)

    def validate_args(self):
        '''
        when user clicks Run button, this method gets called, to Exam if the scedes and output directory info is not empty.
        If empty, prompt an error message box to stop the run. If both are not empty, call run_selection() and continue the run submission.
        Also check the scedes file and outdir existence. If the outdir doesn’t exist, prompt a yes/no message box,
        to ask user create the folder or cancel the process.

        Returns
        -------
        None.

        '''
        self.scedes = self.lbl_scedes.get()
        self.outdir = self.lbl_outdir.get()
        err = ''
        if self.scedes.strip() == '': err += 'You must select a scedes file!\n'
        if self.outdir.strip() == '': err += 'You must select an output directory!\n'
        
        # exam if the scedes file and/or the outdir exist. If not, throw error and block the run
        if not os.path.exists(self.scedes.strip()): err += 'The scedes file not found. Check the scedes entry again!\n'
        if not os.path.exists(self.outdir.strip()):
            answer = messagebox.askyesno('Confirmation','The outdir does not exist. Do you want to create it?\n(Click No to return back to the GUI console)')
            if answer:
                os.mkdir(self.outdir.strip())
            else:
                err += 'The outdir does not exist. Check the outdir entry again!\n'
        
        if err != '':
            messagebox.showerror('ERROR', err)
        else:
            self.run_selection()

    def get_ifortvars_path(self):
        '''
        Read ifortvars.bat file path from $NEMS\scripts\setup\input\nems_config_xilink_nems_ftab.csv and return the value.

        Returns
        -------
        the ifortvars.bat file path

        '''
        import csv
        file = os.path.join(self.root_script_dir,'setup','input','nems_config_xilink_nems_ftab.csv')
        ifortvars_path = ''
        with open(file,newline='') as csvfile:
            rd = csv.DictReader(csvfile)  
            for line in rd:
                if line['key'] == 'IFORTVARS_PATH':
                    ifortvars_path = line['value']
        return ifortvars_path

class DriveSpace():
    def __init__(self):
        self.user = os.environ['USERNAME'].lower()
        self.root_script_dir = os.path.join(os.getcwd(),'scripts')

    def get_available_space_list(self):
        '''
        To provide the required itme list on NEMSApp Drive Space listbox, this call NetwrokDrive class methods to obtain drive space info list and the suggested drive.

        Returns
        -------
        suggested : string
            the most suggested drive path
        drive_list : list
            a list of drive name and its available space info

        '''
        outmenu_file = os.path.join(self.root_script_dir,'setup','input','submit_outmenu.csv')
        net_drive = NetworkDrive()
        suggested, all_drive_dict, num_lastmenu, drive_catg_custom, drive_catg_desired, drive_catg_testing = net_drive.get_catg_drive_list(outmenu_file,self.user)
        suggested = f'{suggested}/output/{self.user}'
        drive_list = net_drive.convert_drive_info_to_display(all_drive_dict)
        return suggested, drive_list

def main():
    ds = DriveSpace()
    suggested_drive, drive_list = ds.get_available_space_list()
    NEMSApp(suggested_drive, drive_list)

if __name__ == '__main__':
    main()
