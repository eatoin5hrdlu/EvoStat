import os
class suppress_stdout_stderr(object):
    '''
    Context manager for "deep suppression" of stdout and stderr
    with suppress_stdout_stderr():
        rogue_function()
    '''
    def __init__(self):
        # Open null files and save stdout(1) and stderr(2)
        self.null_fds =  [os.open(os.devnull,os.O_RDWR) for x in range(2)]
        self.save_fds = (os.dup(1), os.dup(2))

    def __enter__(self):
        # Assign the null pointers to stdout and stderr.
        os.dup2(self.null_fds[0],1)
        os.dup2(self.null_fds[1],2)

    def __exit__(self, *_):
        # Re-assign stdout/stderr back to (1) and (2) and close null files
        os.dup2(self.save_fds[0],1)
        os.dup2(self.save_fds[1],2)
        os.close(self.null_fds[0])
        os.close(self.null_fds[1])
