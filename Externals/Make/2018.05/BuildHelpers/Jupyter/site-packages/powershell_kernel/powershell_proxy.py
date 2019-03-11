import threading
try:
    import queue
except ImportError:
    import Queue as queue
from threading import Timer
from time import sleep

class ReplReader(threading.Thread):
    def __init__(self, repl):
        super(ReplReader, self).__init__()
        self.repl = repl
        self.daemon = True
        self.queue = queue.Queue()
        self.start()

    def run(self):
        r = self.repl
        q = self.queue
        while True:
            result = r.read()
            q.put(result)
            if result is None:
                break

class ReplProxy(object):
    def __init__(self, repl):
        self._repl = repl
        self._repl_reader = ReplReader(repl)
        # this is a hack to detect when we stop processing this input
        self.send_input('function prompt() {"^"}')

        self.stop_flag = False
        self.output = ''
        self.timer = Timer(0.1, self.update_view_loop)
        self.timer.start()
        # get preambula and eveluation of the prompt
        self.get_output()

        self.output_prefix_stripped = True
        self.expected_output_prefix = ''
        self.expected_output_len = 0


    def get_output(self):
        while not self.stop_flag:
            sleep(0.05)
        out = self.output
        self.output = ''
        self.stop_flag = False 
        return out       

    def send_input(self, input):
        # TODO: we should block here until we return output for previous command, should we?

        # for multiline statements we should send 1 extra new line
        # https://stackoverflow.com/questions/13229066/how-to-end-a-multi-line-command-in-powershell
        if '\n' in input:
            input += '\n'

        self.expected_output_prefix = input.replace('\n', '\n>> ') + '\n'
        self.expected_output_len = len(self.expected_output_prefix)
        self.output_prefix_stripped = False

        self._repl.write(input + '\n')

    def handle_repl_output(self):
        """Returns new data from Repl and bool indicating if Repl is still
           working"""
        if self.stop_flag:
            return True
        try:
            while True:
                packet = self._repl_reader.queue.get_nowait()
                if packet is None:
                    return False

                self.write(packet)

        except queue.Empty:
            return True

    def update_view_loop(self):
        is_still_working = self.handle_repl_output()
        if is_still_working:
            self.timer = Timer(0.1, self.update_view_loop)
            self.timer.start()
        else:
            self.write("\n***Repl Killed***\n""")

    def write(self, packet):
        # this is a hack to detect when we stop processing this input
        if packet == '^':
            self.stop_flag = True
            return
        self.output += packet

        if not self.output_prefix_stripped and len(self.output) >= self.expected_output_len:
            if self.output[:self.expected_output_len] != self.expected_output_prefix:
                print("Unexpected prefix: %r : Expected %r" % (
                    self.output[:self.expected_output_len], self.expected_output_prefix
                ))
            else:
                self.output_prefix_stripped = True
                self.output = self.output[self.expected_output_len:]
