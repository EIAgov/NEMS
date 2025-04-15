"""
Created on Jul 14 2023

@author: Claire Su
"""
import shutil, csv, os

class NetworkDrive:
    def get_catg_drive_list(self,outmenu_file,user):
        """Retrieve drive free space info of the drives listed in the outment file ($NEMS\scripts\setup\input\submit_outment.txt), and find
           the suggested drive and return lists of drives by category and dict of all drive info.

        Parameters
        ----------
        outmenu_file : string
            the file path of the outmenu csv file.
        user : string
            user LDAP id

        Returns
        -------
        suggested : string
            the 2 char suggested drive. For example: Q:
        all_drive_dict : dict
            the dict of all drives and their free space info
        num_lastmenu : integer
            the row count of the submit_outmenu.csv content.
        drive_catg_custom : list
            the drive list in the custom categrory.
        drive_catg_desired : list
            the drive list in the important and testing categrories.
        drive_catg_testing : list
            the drive list in the testing category.
        """
        num_lastmenu=0
        # get list of space on drives to store runs on from menu in $NEMS\scripts\setup\input\submit_outmenu.csv and construct a search expression
        drive_catg_custom=[]
        drive_catg_important=[]
        drive_catg_testing=[]
        # desired drives are the important drives plus testing drives
        drive_catg_desired=[]
        with open(outmenu_file,newline='') as csvfile:
             rd = csv.DictReader(csvfile)
             for row in rd:
                 # build drive lists and replace output user path with the user 3-char LADP.
                 num_lastmenu += 1
                 if row['Category'] == 'custom':
                     drive_catg_custom.append({'drive':row['Drive'],'output':row['Output'].replace('user',user),'note':row['Note']})
                 elif row['Category'] == 'important':
                     drive_catg_important.append({'drive':row['Drive'],'output':row['Output'].replace('user',user),'note':row['Note']})
                     drive_catg_desired.append({'drive':row['Drive'],'output':row['Output'].replace('user',user),'note':row['Note']})
                 elif row['Category'] == 'testing':
                     drive_catg_testing.append({'drive':row['Drive'],'output':row['Output'].replace('user',user),'note':row['Note']})
                     drive_catg_desired.append({'drive':row['Drive'],'output':row['Output'].replace('user',user),'note':row['Note']})

        # pack the desired drive names and check their space
        d_list = [d.get('drive') for d in drive_catg_desired]
        all_drive_dict = self.get_network_drive_space(d_list)
        d_list_testing = [d.get('drive') for d in drive_catg_testing]
        suggested = self.get_suggested_drive(all_drive_dict, d_list_testing)

        return suggested, all_drive_dict, num_lastmenu, drive_catg_custom, drive_catg_desired, drive_catg_testing

    def get_network_drive_space(self, drive_list):
        """Retrieve the free space info of the network drives on the passed-in drive list.

        Parameters
        ----------
        drive_list : list
            the list of drives need to retrieve their statistics

        Returns
        -------
        all_drive_dict : dict
            a dict of drive and its free space in GB. For example: {'k':128.99,'s':24.34}
        """
        KB = 1024
        MB = 1024 * KB
        GB = 1024 * MB

        all_drive_dict = {}
        for d in drive_list:
            path = d+':/'
            if os.path.exists(path):
                # get drive free space in GB format
                space = shutil.disk_usage(path).free / GB
                space = round(space,2)
                all_drive_dict[d]=space
            else:
                continue
        return all_drive_dict
    
    def get_suggested_drive(self, all_drive_dict, drive_catg_testing):
        """Calcuate the most available, suggested output drive for user

        Parameters
        ----------
        all_drive_dict : dict
            a dict of drive and its free space in GB. For example: {'k':128.99,'s':24.34}
        drive_catg_testing : list
            the drive list in the testing category.

        Returns
        -------
        suggested : string
            the suggested, most avabiable free space drive. For example: Q:
        """
        max_space = 0
        suggested = 's'
        for d in drive_catg_testing:
            space = all_drive_dict[d]
            # calculate the most free space drive
            if space > max_space:
                max_space = space
                suggested = d
        return suggested.upper()+':'
    
    def convert_drive_info_to_display(self, drive_dict,log_prefix='',is_return_list=True):
        """For UI display, convert the drive dict to uppercase drive and drive space with GB unit. Return in string or list format

        Parameters
        ----------
        drive_dict : dict
            the dict of drives
        log_prefix : str, optional
            with prefix [Setup Program] on cmd console, or without prefix for nems_gui. By default ''
        is_return_list : bool
            True - return in list type. This is design for nems_gui.py the display of the interactive drive space list.
            False - return in string type

        Returns
        -------
        result : str or list
            a long string with line break patching and comma(,) as thousand unit seperateor, to display drive name and its free space in GB. Or, if is_return_list turns on,
            return a list of drive name and its free space in GB.
        """
        result = ''
        for key, value in drive_dict.items():
            value = f'{value:,}GB'
            result += f'{log_prefix}{key.upper()}:{value.rjust(12)}\n'
        if is_return_list:
            return result.split('\n')
        else:
            return result
