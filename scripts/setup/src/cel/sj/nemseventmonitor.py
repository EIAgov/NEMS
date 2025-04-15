from datetime import datetime, date, timedelta
import os
import pandas as pd
import sys
from threading import Thread
import time
import pika
import json

from sj import generate_html_from_dataframe


def watch_file(messagehandler):
    df = messagehandler.getmessagesdf()
    for index, row in df.iterrows():
        if row.Status == 'In queue':
            hosts = ['NEM7', 'NEM8', 'NEM9', 'NEM10']
            for host in hosts:  
                print 
                hostname = "//" + host
                fname = os.path.join(row["Work Directory"].replace("D:",hostname), 'nems_run_status_log.txt')
                if os.path.exists(fname):
                    print(fname)
                    row.Status = 'Running'
                    row.Host = host
                    break
        if row.Status == 'Running':
            # status = row.status
            last_line = ''
            hostname = "//"+row.Host[row.Host.find("@")+1:]
            fname = os.path.join(row["Work Directory"].replace("D:",hostname), 'nems_run_status_log.txt')
            try:
                with open(fname, 'r') as f:
                    last_line = f.readlines()[-1]
                    if last_line == "\n":
                        last_line = f.readlines()[-2]

                # row.status = last_line.strip()
                module = last_line[last_line.lower().find("module:"):]
                row.Module = module[7:module.find(",")].strip()
                cyc = last_line[last_line.lower().find("cycle:"):]
                row.Cycle = cyc[6:cyc.find(",")].strip()
                yr = last_line[last_line.lower().find("year:"):]
                row.Year = yr[5:yr.find(",")].strip()
                itr = last_line[last_line.lower().find("iteration:"):]
                row.Iteration = itr[10:itr.find(",")].strip()
                idx = messagehandler.find_message_history(run_folder=row["Work Directory"])
                if row["Time Submitted"]:
                    time_elapsed = datetime.now() - row['Time Submitted'] 
                    hours, minutes = time_elapsed.seconds // 3600, time_elapsed.seconds // 60 % 60
                    seconds = time_elapsed.seconds - hours*3600 - minutes*60
                    row["Time Elapsed"] = f"{hours}:{minutes}.{seconds,2}"
                messagehandler.updatemessagedf(row.to_dict(), idx)
                messagehandler.export_to_html()
            except FileNotFoundError:
                if 'D:' not in fname:
                    try:
                        with open(fname, 'w') as ns:
                            ns.write('')
                    except:
                        print("Failed to create"  + fname)
                        pass
                else:
                    print("Failed to find file "  + fname)
        #       pass
            except Exception as e:
                if last_line == '':
                    pass
                else:
                    print("Failed to update row " + str(index))
                    print("Row contents: " + str(row.to_dict()))
                    print("Exception: " + str(e))
        # if row.type == 'in_queue':
        #     try:
        #         hostname = ''
        #         lines = ''
        #         with open(os.path.join(row.run_folder, 'nems_run_status_log.txt'), 'r') as f:
        #             lines = f.readlines()
        #         row.type = 'task-received'
        #         row.status = lines[-1]
        #         # hostname = lines[6].split("b'") << probably won't work
        #         row.hostname = hostname[1][:-2]
        #         row.timestamp = datetime.now()
        #         idx = messagehandler.find_message_history(run_folder=row.run_folder)
        #         messagehandler.updatemessagedf(row.to_dict(), idx)
        #         messagehandler.export_to_html()
        #     except Exception as e:
        #         print("Failed to update row " + str(index))
        #         print("Row contents: " + str(row.to_dict()))
        #         print("Exception: " + str(e))

    return df


def run_watch_file(messageHandler):
    while True:
        watch_file(messageHandler)
        # time.sleep(300)
        time.sleep(60)

class MessageHandler(Thread):
    def __init__(self, message_df, outpath = None):
        Thread.__init__(self)
        # self.daemon = True

        cred = pika.PlainCredentials(username="XXXXXXXXXXXX", password="XXXXXXXXXXXX")
        host = "XXXXXXXXXXXX"
        connection = pika.BlockingConnection(pika.ConnectionParameters(host, 5672, '/', credentials=cred))
        self.channel = connection.channel()
        self.message_df = message_df
        if outpath is not None:
            self.outpath = outpath
        else:
            # edit
            #self.outpath = os.path.join("Z:", os.sep, "onl_tst2", "sj_output")
            self.outpath = os.path.join("Y:", os.sep, "RabbitMQ")

    def run(self):

        def callback(ch, method, properties, body):

            message = " [x] Received method '{}', body {}".format(method.routing_key, body.decode())
            print(message)
            loc = self.outpath
            try:
                with open(os.path.join(loc, 'events_log.txt'), 'a') as f:
                    f.write(message)
                    f.write('\n')
            except Exception as e:
                print("Failed to write to log file " + os.path.join(loc, "events_log.txt"))
                print("Exception: " + str(e))

            route_key = method.routing_key
            if 'publish' in route_key:
                msg = json.loads(body.decode())
                self.parse_message(msg)

        self.channel.queue_declare(queue='trace')
        self.channel.queue_bind(exchange='amq.rabbitmq.trace', queue='trace', routing_key="#")
        self.channel.basic_consume(queue='trace', on_message_callback=callback, auto_ack=True)
        print(' [*] Waiting for messages. To exit press CTRL+C')
        self.channel.start_consuming()

    def parse_message(self, msg):
        if type(msg) is list:
            self.messages_in_queue(msg)
        if type(msg) is dict:
            if 'type' in msg:
                message_type = msg['type']
                if message_type == 'task-received':
                    self.messages_started(msg)
                elif message_type == 'task-succeeded':
                    self.messages_finished(msg)
                elif message_type == 'task-failed':
                    self.messages_failed(msg)
            elif 'result' in msg:
                if msg['status'] == 'SUCCESS':
                    self.messages_finished(msg)
                elif msg['status'] == 'FAILURE':
                    self.messages_failed(msg)

    def getmessagesdf(self):
        return self.message_df

    def updatemessagedf(self, new_row, message_df_idx):

        if message_df_idx is None:
            new_row = pd.DataFrame([new_row])
            self.message_df = pd.concat([self.message_df, new_row], ignore_index=True)
        else:
            for key in new_row:
                if new_row[key] is not None:
                    try:
                        self.message_df.loc[message_df_idx, key] = new_row[key]
                    except ValueError:
                        self.message_df.loc[message_df_idx, key] = str(new_row[key])

    def find_message_history(self, run_folder=None, messageid=None, sdp=None):
        # find index of self.message_df
        idx = None
        if self.message_df.empty:
            return idx
        if run_folder:
            # if run_folder in self.message_df['run_folder'].values:
            #     idx = self.message_df[self.message_df['run_folder'] == run_folder].index.values[0]
            if run_folder in self.message_df['Work Directory'].values:
                idx = self.message_df[self.message_df['Work Directory'] == run_folder].index.values[0]
        elif messageid:
            if messageid in self.message_df['messageid'].values:
                idx = self.message_df[self.message_df['messageid'] == messageid].index.values[0]
        elif sdp:
            scen, dkey, part = sdp
            if scen in self.message_df["Scenario"].values:
                sdf = self.message_df[self.message_df["Scenario"] == scen]
                if dkey in sdf["Date Key"].values:
                    ddf = sdf[sdf["Date Key"] == dkey]
                    if part in ddf["Part"].values:
                        pdf = ddf[ddf["Part"] == part]
                        out = pdf["Work Directory"].values[0]
                        idx = self.message_df[self.message_df['Work Directory'] == out].index.values[0]
        return idx

    def messages_in_queue(self, msg):
        out_dir = msg[0][2]
        run_folder = msg[0][1]
        userid = msg[0][0]
        message_type = 'in_queue'
        timestamp = datetime.now()
        print('messages_in_queue')

        # new_row = {'userid': userid, 'run_folder': run_folder, 'start_time': timestamp, 'result': None, 'status': None,
        #            'type': message_type, 'messageid': None, 'hostname': None}
        split = run_folder.split(os.sep)
        if split[-1] in ["p1","p2","p3"]:
            scen = split[-3]
            dkey = split[-2]
            part = split[-1]
        else:
            scen = split[-2]
            dkey = split[-1]
            part = ""
        paridx = self.find_message_history(sdp=(scen,dkey,""))
        if paridx is not None:
            if self.message_df.loc[paridx, "Work Directory"] == run_folder:
                paridx = None
            else:
                userid = self.message_df.loc[paridx,"User ID"]
                self.message_df.loc[paridx,"show"] = False
        new_row = {"User ID": userid, "Scenario": scen, "Date Key": dkey, "Part": part, "Host": "Pending", "Cycle": "",
                   "Year": "", "Iteration": "", "Module": "", "Status": "In queue", "Work Directory": run_folder, "Output Directory": out_dir,'Time Submitted': datetime.now().replace(microsecond=0),'Time Elapsed': "",
                   "messageid": None, "start_time": timestamp, "show": True, "paridx": paridx}
        idx = self.find_message_history(run_folder=run_folder)

        self.updatemessagedf(new_row, idx)
        self.export_to_html()

    def messages_started(self, msg):
        print('messages_running')
        hostname = msg['hostname']
        hostname = hostname[hostname.find("@")+1:]
        messageid = msg['uuid']
        timestamp = msg['timestamp']
        timestamp = datetime.fromtimestamp(timestamp)
        message_type = msg['type']
        args = msg['args'].split('[', 1)[1].split(']')[0].replace("'", '').split(',')
        out_dir = args[2].strip()
        run_folder = args[1].strip()
        userid = args[0].strip()
        # task_name = ['name']
        # retries = msg['retries']
        # new_row = {'userid': userid, 'run_folder': run_folder, 'start_time': timestamp, 'result': None, 'status': None,
        #            'type': message_type, 'messageid': messageid, 'hostname': hostname}
        split = run_folder.split(os.sep)
        if split[-1] in ["p1","p2","p3"]:
            scen = split[-3]
            dkey = split[-2]
            part = split[-1]
        else:
            scen = split[-2]
            dkey = split[-1]
            part = ""
        paridx = self.find_message_history(sdp=(scen,dkey,""))
        if paridx is not None:
            if self.message_df.loc[paridx, "Work Directory"] == run_folder:
                paridx = None
            else:
                userid = self.message_df.loc[paridx,"User ID"]
                self.message_df.loc[paridx,"show"] = False
        status = 'Running'
        if 'D:' not in run_folder:
            status = 'Pending'


        new_row = {"User ID": userid, "Scenario": scen, "Date Key": dkey, "Part": part, "Host": hostname, "Cycle": "",
                   "Year": "", "Iteration": "", "Module": "", "Status": status, "Work Directory": run_folder, "Output Directory": out_dir, 'Time Submitted': None, 'Time Elapsed': "",
                   "messageid": messageid, "start_time": timestamp, "show": True, "paridx": paridx}
        # new_row = [None,pid, hostname, timestamp, None,None, run_folder, message_type]
        idx = self.find_message_history(run_folder=run_folder)
        self.updatemessagedf(new_row, idx)
        self.export_to_html()

    def messages_finished(self, msg):
        print('messages_finished')
        timestamp = 0
        messageid = ''
        hostname = ''
        # instance_id = ''
        if 'task_id' in msg:
            hostname = None
            # instance_id = None
            messageid = msg['task_id']
            timestamp = datetime.now()
            run_time = ''
            message_type = 'task-succeeded'
        else:
            hostname = msg['hostname']
            hostname = hostname[hostname.find("@") + 1:]
            # instance_id = hostname.split('@')[1]
            messageid = msg['uuid']
            timestamp = msg['timestamp']
            # run_time = msg['runtime']
            timestamp = datetime.fromtimestamp(timestamp)
            # message_type = msg['type']

        status = "Finished"
        # result = msg['result']
        if 'result' in msg:
            if str(msg['result']) != '0':
                status = "Failed"

        # new_row = {'userid': None, 'run_folder': None, 'start_time': timestamp, 'result': [result],
        #            'status': 'finished', 'type': message_type, 'messageid': messageid, 'hostname': hostname}
        new_row = {"User ID": None, "Scenario": None, "Date Key": None, "Part": None, "Host": hostname, "Cycle": None,
                   "Year": None, "Iteration": None, "Module": None, "Status": status, "Work Directory": None, "Output Directory": None, 'Time Submitted': None, 'Time Elapsed': "",
                   "messageid": messageid, "start_time": timestamp, "show": None, "paridx": None}
        idx = self.find_message_history(messageid=messageid)
        if idx is not None and idx in self.message_df["paridx"].values:
            for row in self.message_df[self.message_df["paridx"] == idx]["show"].index:
                self.message_df.loc[row,"show"] = False
            new_row["show"] = True

        if idx is not None:
            time_sub = self.message_df['Time Submitted'].iloc[idx]
            if time_sub is not None:
                time_elapsed = datetime.now() - time_sub
                hours, minutes = time_elapsed.seconds // 3600, time_elapsed.seconds // 60 % 60
                seconds = time_elapsed.seconds - hours*3600 - minutes*60
                new_row["Time Elapsed"] = f"{hours}:{minutes}.{seconds,2}"


        watch_file(self)
        self.updatemessagedf(new_row, idx)
        self.export_to_html()

    def messages_failed(self, msg):
        timestamp = 0
        messageid = ''
        hostname = ''
        # instance_id = ''
        error = ''
        if 'task_id' in msg:
            hostname = None
            # instance_id = None
            messageid = msg['task_id']
            timestamp = datetime.now()
            run_time = ''
            message_type = 'task-failed'
            error = msg['result']['exc_type']
        else:
            hostname = msg['hostname']
            hostname = hostname[hostname.find("@") + 1:]
            # instance_id = hostname.split('@')[1]
            messageid = msg['uuid']
            timestamp = msg['timestamp']
            # run_time = msg['runtime']
            timestamp = datetime.fromtimestamp(timestamp)
            # message_type = msg['type']
            # error = msg['exception']

            # result = msg['result']

        # traceback = msg['traceback']
        # new_row = {'userid': None, 'run_folder': None, 'start_time': timestamp, 'result': 'finished',
        #            'status': 'failed', 'type': message_type, 'messageid': messageid, 'hostname': hostname}
        new_row = {"User ID": None, "Scenario": None, "Date Key": None, "Part": None, "Host": hostname, "Cycle": None,
                   "Year": None, "Iteration": None, "Module": None, "Status": "Failed", "Work Directory": None, "Output Directory": None, 'Time Submitted': None, 'Time Elapsed':None,
                   "messageid": messageid, "start_time": timestamp, "show": None, "paridx": None}
        idx = self.find_message_history(messageid=messageid)
        if idx is not None and idx in self.message_df["paridx"].values:
            for row in self.message_df[self.message_df["paridx"] == idx]["show"].index:
                self.message_df.loc[row,"show"] = False
            new_row["show"] = True
        watch_file(self)
        self.updatemessagedf(new_row, idx)
        # if idx:
        #     run_folder = self.message_df.loc[idx, 'run_folder']
        #     if run_folder:
        #         with open(run_folder + '\\error_log.txt', "w") as file:
        #             file.write(error + '\n')
        #             file.write(traceback)

        self.export_to_html()
        print('message failed')

    # def filter_by_type(self, df, type):
    #     df = df.loc[self.message_df['type'] == type]
    #     return df

    def remove_old_messages(self, days=None):
        df = self.message_df
        today = date.today()
        # if days is defined, return a dataframe with data from only the past x days
        days_prior = today - timedelta(days=2)
        if days:
            days_prior = today - timedelta(days=days)
            days_prior_dt = datetime(days_prior.year, days_prior.month, days_prior.day)
            return df.loc[(df['start_time'] >= days_prior_dt) | (df['start_time'] is None)]
        # remove messages older than sunday
        sunday = today - timedelta((today.weekday() + 1) % 7)
        sunday_dt = datetime(sunday.year, sunday.month, sunday.day)
        next_sunday = today + timedelta(weeks=1)
        next_sunday_dt = datetime(next_sunday.year, next_sunday.month, next_sunday.day)
        df = df.loc[(df['start_time'] >= sunday_dt) & (df['start_time'] < next_sunday_dt) | (df['start_time'] is None)]
        return df

    # def set_html_format(self, df):
    #     df = df.copy()
    #     df['start_time'] = df['start_time'].astype('datetime64[ns]').dt.strftime('%c')
    #     in_queue_df = self.filter_by_type(df, 'in_queue')
    #     in_queue_df = in_queue_df[['userid', 'run_folder', 'start_time', 'type']]
    #     started_df = self.filter_by_type(df, 'task-received')
    #     started_df = started_df [['userid', 'start_time', 'status', 'run_folder', 'type', 'messageid', 'hostname' ]]
    #     finished_df = self.filter_by_type(df,'task-succeeded')
    #     failed_df = self.filter_by_type(df, 'task-failed')
    #     finished_df = pd.concat([finished_df, failed_df], ignore_index=True)
    #     # should there be a task failed type, should we concat failed and finished??
    #     html = htmlf()
    #     html.set_table(in_queue_df,'in_queue_df')
    #     html.set_table(started_df,'started_df')
    #     html.set_table(finished_df,'finished_df')
    #     html.queue_length = len(in_queue_df)
    #     html.compile_html()
    #     return html
    # html.save_html("weps_run_monitor.html")

    def export_to_html(self):
        # get only messages for this past week
        self.message_df = self.remove_old_messages()
        messages_df_days = self.remove_old_messages(7) # enter the number of days
        # html_week = self.set_html_format( self.message_df)
        # html_days = self.set_html_format(messages_df_2days)
        trim_df_week = self.message_df[self.message_df["show"] == True].drop(["messageid","start_time","paridx","show"],axis=1)
        trim_df_days = messages_df_days[messages_df_days["show"] == True].drop(["messageid","start_time","paridx","show"],axis=1)

        # fn = r'Z:\Teams\Project\WEPS\celery_html_files/'
        loc = self.outpath
        # html_week.save_html(fn + "weps_run_monitor_week.html")
        # html_days.save_html(fn + "weps_run_monitor.html")
        generate_html_from_dataframe(trim_df_week, os.path.join(loc, "nems_run_monitor_week.html"))
        generate_html_from_dataframe(trim_df_days, os.path.join(loc, "nems_run_monitor.html"))


if __name__ == '__main__':

    messages_df = pd.DataFrame(
        # columns=['userid', 'run_folder', 'start_time', 'result', 'status', 'type', 'messageid', 'hostname'])
        columns=['User ID', 'Scenario', 'Date Key', 'Part', 'Host', 'Cycle', 'Year', 'Iteration', 'Module', 'Status', 'Work Directory','Output Directory', 'Time Submitted', 'Time Elapsed',
                 'messageid', 'start_time', 'show', 'paridx'])

    mh = MessageHandler(messages_df, sys.argv[1] if len(sys.argv) > 1 else None)

    x = Thread(target=run_watch_file, args=(mh,), daemon=False)
    try:
        mh.start()
        x.start()
    except KeyboardInterrupt:
        print('Interrupted')
        try:
            sys.exit()
        except SystemExit:
            os._exit()
