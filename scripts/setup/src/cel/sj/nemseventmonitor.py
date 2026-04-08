"""
Created on Aug 12 2024
Modified Oct 20 2025

@author: janea.dixon
@author: gregory.miller
"""

from datetime import datetime, date, timedelta
import os
import pandas as pd
import sys
from threading import Thread
import time
import pika
import json
import argparse
from sj import generate_html_from_dataframe
import sqlite3

MYHOST = os.environ['COMPUTERNAME']
SECRETS_FILE = "O:\\python_environments\\secrets.json"
if "test" in MYHOST.lower() or "tst" in MYHOST.lower():
    MODE = "TEST"
else:
    MODE = "NEMS"

def watch_file(messagehandler):
    """Watches the nems_run_status_log.txt file for updates.
    
    This function reads the content of the nems_run_status_log.txt file, parses its content
    and updates the dataframe accordingly. The nems_run_status_log.txt file location is generated
    from the work directory field of the dataframe. The file contains various information
    about run status such as current model, year and iteration.

    Args:
        messagehandler (MessageHandler): An instance of the MessageHandler class.
    
    Returns:
        pandas.DataFrame: Updated Pandas dataframe.
    """
    df = messagehandler.getmessagesdf()
    for index, row in df.iterrows():
        if row.Status == 'In queue':
            hosts = ['NEM7', 'NEM8', 'NEM9', 'NEM10', 'TSTNEM1']
            for host in hosts:  
                hostname = "//" + host
                fname = os.path.join(row["Work Directory"].replace("D:",hostname), 'nems_run_status_log.txt')
                if os.path.exists(fname):
                    row.Status = 'Running'
                    row.Host = host
                    break
        if row.Status == 'Running':
            last_line = ''
            hostname = "//"+row.Host[row.Host.find("@")+1:]
            fname = os.path.join(row["Work Directory"].replace("D:",hostname), 'nems_run_status_log.txt')
            try:
                with open(fname, 'r') as f:
                    last_line = f.readlines()[-1]
                    if last_line == "\n":
                        last_line = f.readlines()[-2]
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
                    days = time_elapsed.days
                    hours = time_elapsed.seconds // 3600
                    minutes = (time_elapsed.seconds % 3600) // 60
                    seconds = time_elapsed.seconds % 60
                    if days > 0:
                        row["Time Elapsed"] = f"{days} day{'s' if days > 1 else ''}, {hours:02}:{minutes:02}:{seconds:02}"
                    else:
                        row["Time Elapsed"] = f"{hours:02}:{minutes:02}:{seconds:02}"
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
            except Exception as e:
                if last_line == '':
                    pass
                else:
                    print("Failed to update row " + str(index))
                    print("Row contents: " + str(row.to_dict()))
                    print("Exception: " + str(e))
    return df


def run_watch_file(messageHandler):
    while True:
        watch_file(messageHandler)
        time.sleep(60)

class MessageHandler(Thread):
    def __init__(self, message_df, outpath = None, dump_file = None, table_name = "nems_runs"):
        Thread.__init__(self)

        # setup paths for logging and data storage
        if outpath is not None:
            self.outpath = outpath
        else:
            self.outpath = os.path.join("Y:", os.sep, "RabbitMQ")
        if dump_file is not None:
            self.dump_file_path = os.path.join(self.outpath, os.sep, dump_file)
        else:
            self.dump_file_path = os.path.join(self.outpath, os.sep, "nems_run_log.db")
        self.table_name = "nems_runs"
        # TODO GNM add a check if the path and file above exist

        # Configure Pika connection for RabbitMQ server
        with open(SECRETS_FILE, encoding='UTF-8', mode='r') as s:
            t = json.load(s)
        host =  t[MODE]['RabbitMQ']['HOST']
        password = t[MODE]['RabbitMQ']['SECRET']
        user = t[MODE]['RabbitMQ']['USER']
        cred = pika.PlainCredentials(username=user, password=password)
        conn_parms = pika.ConnectionParameters(host, 5672, '/', credentials=cred)
        connection = pika.BlockingConnection(conn_parms)
        self.channel = connection.channel()

        # dataframe for all run data
        self.message_df = message_df
        
        # variables for tracking and dumping dataframe to disk
        self.captured_message_ids = self._get_message_ids_from_db()
        self.allowed_statuses = ['finished', 'failed', 'completed', 'error']        

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
        idx = self.find_message_history(run_folder=run_folder)
        self.updatemessagedf(new_row, idx)
        self.export_to_html()

    def messages_finished(self, msg):
        print('messages_finished')
        timestamp = 0
        messageid = ''
        hostname = ''
        if 'task_id' in msg:
            hostname = None
            messageid = msg['task_id']
            timestamp = datetime.now()
            run_time = ''
            message_type = 'task-succeeded'
        else:
            hostname = msg['hostname']
            hostname = hostname[hostname.find("@") + 1:]
            messageid = msg['uuid']
            timestamp = msg['timestamp']
            timestamp = datetime.fromtimestamp(timestamp)

        status = "Finished"
        if 'result' in msg:
            if str(msg['result']) != '0':
                status = "Failed"
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
                days = time_elapsed.days
                hours = time_elapsed.seconds // 3600
                minutes = (time_elapsed.seconds % 3600) // 60
                seconds = time_elapsed.seconds % 60
                if days > 0:
                    new_row["Time Elapsed"] = f"{days} day{'s' if days > 1 else ''}, {hours:02}:{minutes:02}:{seconds:02}"
                else:
                    new_row["Time Elapsed"] = f"{hours:02}:{minutes:02}:{seconds:02}"

        watch_file(self)
        self.updatemessagedf(new_row, idx)
        self.export_to_html()
        self.dump_row_on_event(messageid=messageid)

    def messages_failed(self, msg):
        timestamp = 0
        messageid = ''
        hostname = ''
        error = ''
        if 'task_id' in msg:
            hostname = None
            messageid = msg['task_id']
            timestamp = datetime.now()
            run_time = ''
            message_type = 'task-failed'
            error = msg['result']['exc_type']
        else:
            hostname = msg['hostname']
            hostname = hostname[hostname.find("@") + 1:]
            messageid = msg['uuid']
            timestamp = msg['timestamp']
            timestamp = datetime.fromtimestamp(timestamp)
        
        idx = self.find_message_history(messageid=messageid)
        if idx is not None:
            module = self.message_df["Module"].iloc[idx]
        else:
            module = None

        my_status="Failed"
        if module and 'stop' in module.lower():
            my_status = "Stopped"

        new_row = {"User ID": None, "Scenario": None, "Date Key": None, "Part": None, "Host": hostname, "Cycle": None,
                   "Year": None, "Iteration": None, "Module": None, "Status": f"{my_status}", "Work Directory": None, "Output Directory": None, 'Time Submitted': None, 'Time Elapsed':None,
                   "messageid": messageid, "start_time": timestamp, "show": None, "paridx": None}
        
        if idx is not None and idx in self.message_df["paridx"].values:
            for row in self.message_df[self.message_df["paridx"] == idx]["show"].index:
                self.message_df.loc[row,"show"] = False
            new_row["show"] = True
        watch_file(self)
        self.updatemessagedf(new_row, idx)
        self.export_to_html()
        self.dump_row_on_event(messageid=messageid)
        print('message failed')

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

    def export_to_html(self):
        # get only messages for this past week
        self.message_df = self.remove_old_messages()
        messages_df_days = self.remove_old_messages(7) # enter the number of days
        trim_df_week = self.message_df[self.message_df["show"] == True].drop(["messageid","start_time","paridx","show"],axis=1)
        trim_df_days = messages_df_days[messages_df_days["show"] == True].drop(["messageid","start_time","paridx","show"],axis=1)
        loc = self.outpath
        generate_html_from_dataframe(trim_df_week, os.path.join(loc, "nems_run_monitor_week.html"))
        generate_html_from_dataframe(trim_df_days, os.path.join(loc, "nems_run_monitor.html"))

    def dump_row_on_event(self, messageid):
        message_row = pd.DataFrame()
        if messageid and messageid not in self.captured_message_ids:
            # copy the row from the dataframe
            message_row = self.message_df[
                (self.message_df['messageid'] == messageid) &
                (self.message_df['Status'].str.lower().isin(self.allowed_statuses))
            ].copy()
        
        try:
            if message_row['User ID'] is None or message_row['Work Directory'] is None:
                return 
        except ValueError as e:
            return
        except KeyError as e:
            return
        except Exception as e:
            print(f"uncaught exception in dump_row_on_event(): {e}")
            return
        
        if not message_row.empty:
            self._dump_dataframe_to_db(message_row)
        
    def _load_dataframe_from_db(self, initial_df):
        if os.path.exists(self.dump_file_path):
            try:
                conn = sqlite3.connect(self.dump_file_path)
                loaded_df = pd.read_sql_query(f"SELECT * FROM {self.table_name}", conn, parse_dates=['Time Submitted', 'start_time'])
                
                # match the column order to the input and handle any missing columns
                for col in initial_df.columns:
                    if col not in loaded_df.columns:
                        loaded_df[col] = None
                return loaded_df[initial_df.columns]
                conn.close()
            except Exception as e:
                # if above failed return empty df to mimic older behavior
                print("Failed ot load data from database. {e}.")
                return initial_df.copy()
        else:
            print("SQLite DB not found. Initializing new file.")
            os.makedirs(os.path.dirname(self.dump_file_path), exist_ok=True)
            return initial_df.copy()
        
    def _get_message_ids_from_db(self):
        if os.path.exists(self.dump_file_path):
            try:
                conn = sqlite3.connect(self.dump_file_path)
                message_ids_df = pd.read_sql_query(f"SELECT messageid FROM {self.table_name} WHERE messageid is NOT NULL",
                                                   conn)
                conn.close()
                return set(message_ids_df['messageid'].dropna().unique())
            except Exception as e:
                print(f"Failed to load captured message IDs from DB. {e}. Beware duplicate rows in DB.")
                return set()
        return set()

    def _dump_dataframe_to_db(self, source_df):
        if source_df.empty:
            return
        
        out_df = source_df.copy()
        
        if 'start_time' in out_df.columns:
            out_df = out_df.drop(columns=['start_time'])

        # format times for sqlite compatibility
        for col in ['Time Submitted']:
            if col in out_df.columns:
                out_df[col] = pd.to_datetime(out_df[col], errors='coerce')
                out_df[col] = out_df[col].apply(lambda x: x.strftime('%Y-%m-%d %H:%M:%S') if pd.notna(x) else None)

            try:
                conn = sqlite3.connect(self.dump_file_path)
                cursor = conn.cursor()

                column_defs = []
                for col, dtype in out_df.dtypes.items():
                    sql_type = 'TEXT'
                    if pd.api.types.is_integer_dtype(dtype):
                        sql_type = 'INTEGER'
                    elif pd.api.types.is_float_dtype(dtype):
                        sql_type = 'REAL'
                    elif col in ['Time Submitted', 'start_time']:
                        sql_type = 'DATETIME'
                    elif pd.api.types.is_bool_dtype(dtype):
                        sql_type = 'BOOLEAN'

                    if col == "messageid":
                        column_defs.append(f'"{col}" {sql_type} PRIMARY KEY')
                    else:
                        column_defs.append(f'"{col}" {sql_type}')

                create_table_stmt = f"""
                CREATE TABLE IF NOT EXISTS {self.table_name} (
                    {", ".join(column_defs)}
                );
                """
                cursor.execute(create_table_stmt)
                conn.commit()

                # setup a prepared statement to insert rows
                cols = ", ".join(f'"{col}"' for col in out_df.columns)
                placeholders = ", ".join(['?' for _ in out_df.columns])
                update = ", ".join(f'"{col}" = EXCLUDED."{col}"' for col in out_df.columns if col != "messageid")
                
                upsert_stmt = f"""INSERT INTO {self.table_name} ({cols})
                                VALUES ({placeholders})
                                ON CONFLICT ("messageid") DO UPDATE SET
                                {update};"""
                
                for _, row in out_df.iterrows():
                    cursor.execute(upsert_stmt, row.values.tolist())
                conn.commit()
                conn.close()
                print(f"Dumped {len(out_df)} to {self.dump_file_path}")

                for mid in out_df['messageid'].dropna().unique():
                    self.captured_message_ids.add(mid)

            except Exception as e:
                print(f"Failed to dump messages to DB. {e}")
                if conn in locals() and conn:
                    conn.rollback()
                    conn.close()
            return

    def _dump_dataframe_to_csv(self, source_df):
        if source_df.empty:
            return
        
        out_df = source_df.copy()

        # dump to disk
        if os.path.exists(self.dump_file_path):
            out_df.to_csv(self.dump_file_path, mode='a', header=False, index=False)
        else:
            out_df.to_csv(self.dump_file_path, mode='w', header=True, index=False)
        print(f"Dumped {len(out_df)} messages to {self.dump_file_path}.")
        
        # capture id to avoid dupes
        for mid in out_df['messageid'].dropna().unique():
            self.captured_message_ids.add(mid)
        return

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="NEMS Run Monitor")
    parser.add_argument("--outpath", "-o", type=str, default="Y:/RabbitMQ/", 
                        help="Optional path to set the output directory.")
    parser.add_argument("--dbpath", "-d", type=str, default="Y:/RabbitMQ/nems_runs.db", 
                        help="Path to the SQLite file used for logging runs. Defaults to Y:/RabbitMQ/nems_runs.db.")
    args = parser.parse_args()

    welcome_str = "NEMS Run Monitor\r\n----------------\r\nNEMS run monitor is starting up.\r\n"
    welcome_str += f"Run Monitor is in {MODE} mode.\r\nUsing DB file {args.dbpath}."

    messages_df = pd.DataFrame(
        columns=['User ID', 'Scenario', 'Date Key', 'Part', 'Host', 'Cycle', 'Year', 'Iteration', 'Module', 'Status', 'Work Directory','Output Directory', 'Time Submitted', 'Time Elapsed',
                 'messageid', 'start_time', 'show', 'paridx'])

    mh = MessageHandler(messages_df, outpath=args.outpath, dump_file=args.dbpath)
    print(welcome_str)
    x = Thread(target=run_watch_file, args=(mh,), daemon=False)
    try:
        mh.start()
        x.start()
    except KeyboardInterrupt:
        print('Interrupted')
        try:
            sys.exit(1)
        except SystemExit:
            os._exit(1)
