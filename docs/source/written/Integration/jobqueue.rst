Managing NEMS runs
=====================

System Design
-------------

RabbitMQ is an open-source message passing broker running the `Advanced
Message Queuing
Protocol <https://en.wikipedia.org/wiki/Advanced_Message_Queuing_Protocol>`__
(AMQP).
`Celery <https://docs.celeryq.dev/en/stable/getting-started/introduction.html>`__
is a Python-based system for setting up and running task queues that can
use `RabbitMQ <https://www.rabbitmq.com/>`__ as the message broker.

The NEMS job queue has the following features:

- A Celery client, called by the user, which handles the initial model
  setup for the NEMS run. After setup, the Celery client sends a message
  through the broker to the worker via the Celery task command.

- Celery workers on each of the three dedicated servers, to perform the
  actual execution of the NEMS runs. Celery workers act as consumers of
  messages from the RabbitMQ Server.

- A run monitor accessible by users. This monitor shows the status of
  each run, including user, scenario name, datekey, part (if
  applicable), host, cycle number, year, iteration number, status, and
  output directory. The monitor maintains data for a configurable time –
  by default, it outputs files with two-day and one-week retention
  periods – and is searchable and sortable.

We have stood up a RabbitMQ broker with queues for processing NEMS run
job requests. The queue structure is discussed in the following section.
We have also developed Python worker and client scripts to wrap around
the current NEMS scripts and initiate a NEMS run, along with a run
monitor script to generate files which users can read to track run
progress.

The key elements of the resulting system are thus:

- **Run Monitor Front End:** HTML files which can be opened in any web
  browser to display a searchable, sortable list of ongoing and
  completed runs.

- **Run Monitor Back End:** A continuously-running Python script which
  regularly regenerates the aforementioned HTML files to remain
  up-to-date with run status.

- **RabbitMQ Server:** A broker service running the AMQP protocol. The
  Celery clients submit messages to this server to request execution of
  a NEMS run. Messages are made available to the connected Celery
  workers on the dedicated NEMS machines to execute the run, distributed
  by workload and availability.

- **Celery Workers:** Celery workers are to be deployed on the three
  dedicated servers, where actual execution of NEMS runs will occur.
  Celery workers act as consumers of messages from the RabbitMQ server.

Queue Structure
---------------

NEMs may be executed in either of two ways, either as a single
executable or loosely coupled parallel structure. In the former
incarnation (jognems), each module is called in sequence until the run
is completed. In the latter, only a subset of modules is executed in
each of two partitions (P1 and P2). The output of P1 and P2 is then
merged into a third partition (P3).

When designing the structure of queues through which tasks would flow,
it was desired that each part of a parallel NEMS (parnems) run should be
executed as its own task. Accordingly, and with the aim of maintaining
similarity in execution between parnems and jognems runs, we devised the
following arrangement (using a parnems run as an example):

**Private Worker**

Worker executes P1/P2/P3

Send: P[#] Complete

**Workstation**

Executes runnems.bat and initiates parnems run

**RABBITMQ Server**

Queue: shared

Queue: Worker

**Shared Worker**

Worker initiates cycle.py

Send: Start Run

Receive: Start Run

Send: Start P[#]

Receive: Start P[#]

Figure 5. Queue Process Flow

Each machine that executes NEMS runs operates two Celery workers to
receive tasks. One worker listens to a queue named “shared” and used by
all workers, and worker listens to the queue named for the COMPUTERNAME
environment variable of its host machine. All runs will initially be
sent to the “shared” queue, from which they can be fetched by any worker
based on availability.

Once a worker has received the overall task for a run, it is necessary
for all tasks within that run to be managed by that same worker, so that
there is no need to repeatedly transfer run files among machines. As
such, in the case of a parnems run, the worker will send tasks for parts
1 and 2 of the run (P1 and P2) to the COMPUTERNAME queue, on which only
that worker is listening. This ensures that the same worker will receive
and execute the P1 and P2 tasks. Once these are completed, a part 3 (P3)
task will be sent in the same way. In the case of a jognems run, instead
only a single task will be sent to the COMPUTERNAME queue for the entire
cycle. In either case, the process will then be repeated as many times
as required by the parameters of the run.

In addition to its utility for run-monitoring and similar purposes, this
design can easily be adjusted to allow subtasks to be shared across some
or all of the relevant machines, in case a future system will have runs
executed on shared drives such that file transfer overhead will no
longer be a concern. If a worker’s setup is adjusted such that it is
also listening to the COMPUTERNAME queue(s) of one or more other
machines, it will be permitted to execute any subtask sent by those
computers. Alternatively, with appropriate code changes, it would also
be possible to bring all workers exclusively onto the shared queue,
allowing any one machine to receive tasks sent by any other. These
possible changes will be discussed in more detail in the following
section.

Cycle.py
~~~~~~~~

The script cycle.py executes with a celery message to run nems_flow on a
workers . The Python script run_task.py is used to send the message to
the COMPUTERNAME queue.

Tasks.py
~~~~~~~~

The Python program tasks.py first establishes a connection with the
RabbitMQ server. It then defines the Celery task exec_at_loc, which is
used by the worker to execute cycle.sh and nems.exe. This task is a
function which takes three parameters: userid (User ID), loc (location),
and comm (command). The user ID is only passed to facilitate tracking by
the run monitor and is not actually used by the task itself.

First, exec_at_loc checks if the location provided is in D:/workdir. If
it is, then the script simply executes the command provided at that
location. The task then waits for this subprocess to complete and
returns a code reporting its completion status.

If the location provided is not in D:/workdir, this indicates the
selected output directory for the NEMS run is the one that was created
in the initial setup by nems_setup.py. Accordingly, scenario and datekey
are determined using the final elements of this path and all files are
copied from it into D:/workdir/[scenario]/[datekey]. This folder is
created if it did not exist or replaced if it did (though the latter
should never be necessary, as all scenario-datekey pairs should be
unique). The command provided is then executed at this new location, as
above. Afterwards, cleanup is performed: the scedes.all file, all input
folders, all .dll files, and all .exe files are removed from the new
location if present, though if ftab.exe exists it is first copied to
ftab.xxx. The contents of the cleaned-up folder are then copied back to
the originally-provided location, overwriting where necessary, and then
deleted from D:/workdir once the copying is complete. Finally, as
before, the task returns the return code of the subprocess responsible
for command execution.

Worker_start.bat
~~~~~~~~~~~~~~~~

The worker_start.bat batch script launches a Celery worker. It first
ensures that the correct Python environment is active, so that the
necessary packages to run the worker will be available, and then
launches a worker using tasks.py. It is also set to use a pool of
threads to execute a maximum number of concurrent tasks; once that limit
is reached, no new tasks will be accepted until a slot is freed up by a
task completing.

run_task.py
~~~~~~~~~~~

The Python program run_task.py provides a single function, run_task.
This function takes three arguments: loc (location), comm (command), and
q (queue). It establishes a Celery connection to a RabbitMQ server –
currently configured to be on ASHTSTNEMVIR002 with username “user” and
password “test”. It then sends a message to the specified queue to run
exec_at_loc, from tasks.py, using the provided location and command. It
also gets the value of the USERNAME environment variable and passes this
to exec_at_loc; again, this is for tracking purposes only, as
exec_at_loc does not actually use user ID for any direct purpose. The
results of this task are then returned.

If run_task.py is launched as a Python script directly, then it will
execute run_task using the command line arguments provided to it as
input. In this case, it will complete with a return code equal to the
results of this function. Since the run_task returns the results of the
task, and the exec_at_loc returns the return code of the subprocess used
to execute its provided command, this will propagate any errors in
command execution back to the place where run_task.py was called.

Run Monitor
-----------

The run monitor is composed of a pair of HTML files – one for records up
to two days old, one for those up to one week old – which are generated
and regularly updated by a Python script kept continuously running to
monitor for changes. This script takes its input from two primary
sources. For tracking when runs begin or end, it gets copies of all
messages in all queues from the RabbitMQ server, using its “firehose”
feature. For tracking the status of these runs while they are in
progress, it reads the nems_run_status_log.txt file generated by
nems.exe, each line of which specifies the module, cycle, year, and
iteration, with the final one being the most current.

This is achieved through the use of three scripts:
eventmonitor_start.bat, nemseventmonitor.py, and sj.py. The program
eventmonitor_start.bat launches nemseventmonitor.py, which processes the
aforementioned input into a dataframe. The dataframe is passed to sj.py
for conversion into HTML output. Taken all together, the resulting
process flow is shown in the figure below:

**RABBITMQ SERVER**

Firehose copies all messages from all queues

Firehose

**RUN MONITOR**

Stores all run status info in a dataframe

Pika reads incoming messages, detects run entering shared queue, jognems
run starting or finishing on worker, parnems part starting or finishing
on worker, run finishing overall

Run monitor reads log files to get status of all tracked runs, updates
periodically

**Workstation**

NEMS run reports current status in its log file(s)

Log file(s)

**SJ.PY**

Data is saved into HTML files in common location

Dataframe

**USER**

User reads HTML files in browser

HTML

Figure 6. Show Jobs Process Flow

Much like worker_start.bat, eventmonitor_start.bat is a simple script
which activates the correct Python environment and then runs
nemseventmonitor.py. It provides a single argument to
nemseventmonitor.py: the desired location for the output HTML files.

The Python program nemseventmonitor.py was adapted from the World Energy
Projection System (WEPS) event monitor. It consists of two threads: one
watches for incoming messages from the firehose, while the other
periodically checks for updates to the status log file of every
currently-tracked run in progress. Both threads update a shared
dataframe and send it to sj.py for conversion to HTML output. This
output is written to a location specified by the first command line
argument given when launching the script.

To track incoming messages, a `Pika
connection <https://pika.readthedocs.io/en/stable/modules/connection.html>`__
is established to the RabbitMQ server. A queue “trace” is established to
receive messages from the exchange “amq.rabbitmq.trace”, which is where
the firehose publishes its message copies. A processing function is then
set to execute whenever a message is received in the queue. This
function logs the received message to a file events_log.txt in the
output directory, then updates the dataframe based on its contents.
Messages are sorted by their header info into five groups: no update
needed, task added to queue, task started, task succeeded, and task
failed.

For tasks added to the queue, the message body is parsed to determine
user ID, scenario, datekey, part (if applicable), and run folder.
Additionally, a check is performed to determine if this task is a
subtask of an existing run. If it is, then the user ID is updated to
that of the parent task, as subtasks will initially have the user ID of
the user who launched the Celery worker instead of the user who launched
the run. If the task is a subtask, a flag will also be set to hide its
parent task in the run monitor to avoid displaying redundant rows. In
either case, the dataframe is updated with a row containing the
extracted values, a host name of “Pending”, a status of “In queue”, and
a timestamp of the current time. If a row already exists with the same
run folder, that row is overwritten; otherwise, a new row is added. The
dataframe is then sent to be exported in HTML form.

For tasks started, the process is very similar. The host machine name is
also obtained from the message body, as is the message ID. The latter is
not displayed and is used only for internal tracking of which messages
are associated with the same task. The Status attribute is written as
“Running” rather than “In queue”. Otherwise, all steps are as described
for tasks added to queue.

For successful and failed tasks, only the host name and message ID are
obtained from the message body. A check is performed to determine if
this task is a parent to any subtasks – if so, the flag to hide it in
the run monitor is unset, and instead all of its subtasks are flagged to
be hidden. Then the message ID is used to find a matching row, which is
updated with the extracted values and a timestamp of the current time,
along with the status of “Finished” (for successful tasks) or “Failed”
(for failed tasks). A row should always be found, since a task cannot
succeed or fail without first being started; however, if none is, a new
one will be generated with “None” in all remaining columns. Finally, a
manual call is made to the function which checks for status log updates
– only currently-running tasks are checked for updates, so this ensures
that all values are correct before they become locked in. The dataframe
is then sent to be exported in HTML form.

The thread responsible for tracking status log files runs a function to
check for updates once per minute; this time can be increased or
decreased by adjusting the value passed to time.sleep. For each row in
the dataframe with status “Running”, the function uses the host name and
run folder recorded for that row to determine where the task in question
is running, then looks for a status log file named
“nems_run_status_log.txt” in that directory. If this file is found, its
last line is extracted and parsed for cycle, year, and iteration, which
are then used to update the relevant row of the dataframe. Once all rows
have been checked, the dataframe is sent to be exported in HTML form.

When the dataframe is sent for export, it is first cleared of all
messages older than the past week. A more recent copy is then made
containing only data from the past two days, though this does not affect
the overall data stored. The former behavior can be adjusted by editing
the function remove_old_messages, while the latter can be adjusted by
passing it a different argument when creating the copy. Copies are then
made of both dataframes without information which does not need to be
displayed – rows flagged for hiding, the flag which determines this,
message ID, timestamp, and parent task. These copies are sent to sj.py
for conversion into HTML files in the output folder.

sj.py contains a function, generate_html_from_dataframe, which takes a
dataframe and generates an HTML file based on its contents. The file
displays the same columns as are contained in the dataframe. Clicking on
the header of any of these columns sorts the display by that column, and
there is a search box for the first column, which will be user ID unless
any changes are made. Reloading the page will clear all sorting and
filtering.

Operation
---------

First, an appropriately-configured RabbitMQ server is required. In order
to establish a new server, the following steps are necessary:

1. Install `Erlang <https://www.erlang.org/>`__ (if not already present)
   and RabbitMQ on the server-to-be, following the installation
   instructions from the RabbitMQ website. Note in particular that, if
   using a non-administrative account, it will be necessary to copy the
   file .erlang.cookie from system32/config/systemprofile to the user’s
   home path.

2. Ensure that the RabbitMQ Windows service is running. It should launch
   automatically upon initial installation.

3. Ensure that access to port 5672 is permitted through the server’s
   firewall.

4. Using a command prompt, navigate to the sbin folder in RabbitMQ’s
   install location and set up a new user using the following command
   (sans double quotes): “rabbitmqctl.bat add_user ‘test’ ‘password’”.
   This will create a new user profile with username “test” and password
   “password” on the server – if a different username or password is
   desired, simply replace the relevant field in the command, while
   maintaining its surrounding quotation marks and escaping any
   necessary characters.

5. To enable the event monitor to pick up events from the server, also
   activate RabbitMQ’s “firehose” feature by running the following
   command (again, sans double quotes): “rabbitmqctl.bat trace_on”. Note
   that this command in particular must be run again every time the
   server is restarted.

To launch a Celery worker, ensure that tasks.py and worker_start.bat are
located in the same directory, then run worker_start.bat from a command
prompt. The worker can then be stopped by the ctrl-c keyboard shortcut
or by closing its command prompt window. While running, unless this
behavior has been modified, it will log all events to workerlog.txt
including its startup messages, any tasks received or completed, and any
errors encountered. Unless making modifications such as the proposal
above regarding separating concurrency limits by queue, exactly one such
worker should be launched on each machine which will be hosting NEMS
runs, from a user account with all necessary permissions to complete
those runs.

To launch a NEMS run through Celery, ensure that the program run_task.py
is present in the scripts/setup/src/cel folder, or if it is to be
relocated ensure that nems_setup.py has its import and shutil.copy
statements modified accordingly. Additionally, ensure that the variable
NEMSPYENV is set to an environment with the Celery package and all
dependencies installed – this should be done both for the environment
variable through a shell command and in the scedes file to be used.
These prerequisites being met, simply execute runnems.bat from a command
line as usual and make all appropriate selections. A command prompt
window will be launched, per previous behavior. It is not recommended to
close this command prompt window until the run has been completed.

To operate the run monitor, it is necessary to leave a single instance
of nemseventmonitor.py running continuously, which will keep the output
HTML file up to date as it receives new events from the RabbitMQ server.
This instance can be located on any computer and account with access and
permissions to modify files in the desired output directory, which is
set to Z:/onl_tst2/sj currently. To launch it, execute
eventmonitor_start.bat in a command prompt window.

It is recommended that the run monitor be launched immediately after
executing the “rabbitmqctl.bat trace_on” command on the RabbitMQ server
and before any runs are launched; any runs which started before
launching the monitor may not be properly tracked and may generate junk
output. If the run monitor is launched while previous HTML output files
exist in the target directory, it will overwrite them, so be sure to
back up any previous monitor output which needs preservation before
launching a new instance. This behavior can also be used to clean up any
junk output generated as a result of runs executed prior to the run
monitor’s launch; however, any runs which were started before launching
the run monitor, and which are still running, may generate new junk
output upon completion of subtasks and/or of the run itself.

The output of the run monitor will be two HTML files, one which discards
old records after two days and one which discards them after a week.
These files can be read using any web browser, and refreshing the page
will update it for any changes have occurred since it was first opened.
The run monitor checks for log file updates at an interval of once per
minute, as well as automatically updating whenever a message is received
from the RabbitMQ server indicating that a new task has started or has
been completed. Clicking on column headers permits sorting in ascending
or descending order by the value of that column, though any sorting will
not be preserved through a page update.