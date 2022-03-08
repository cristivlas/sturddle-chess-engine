"""
Sturddle Chess Engine (c) 2022 Cristi Vlasceanu.
-------------------------------------------------------------------------

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------

"""
import logging
import threading
from collections import deque

__IN__, __OUT__ = 0, 1


class QueueFull(Exception):
    pass


class Locking:
    def __init__(self):
        self._lock = threading.RLock()

    def synchronized(f):
        def inner(self, *args):
            with self._lock:
                return f(self, *args)
        return inner

"""
Helper class for quick-and-dirty pondering on a background thread.
"""
class WorkerThread(Locking):
    def __init__(self):
        super().__init__()
        self.__thread = threading.Thread(target=self.__main)
        self.__thread.daemon = True
        self.__queues = (deque(), deque())  # in / out
        self.__events = (threading.Event(), threading.Event())
        self.__active = True
        self.__paused = False
        self.__thread.start()

    @Locking.synchronized
    def pop(self, inout):
        return self.__pop(inout)

    def __pop(self, inout):
        queue = self.__queues[inout]
        event = self.__events[inout]
        m = queue.popleft() if queue else None
        if not queue:
            event.clear()
        return m

    @Locking.synchronized
    def __put_message(self, inout, m, max_count=None):
        if inout == __IN__:
            if self.__paused:
                return
        queue = self.__queues[inout]

        if max_count is None or len(queue) < max_count:
            queue.append(m)
            self.__events[inout].set()
        else:
            raise QueueFull()

    def __get_message(self, inout):
        while True:
            self.__events[inout].wait()
            m = self.pop(inout)
            if m is not None:
                return m

    @Locking.synchronized
    def messages(self):
        while True:
            msg = self.__pop(__OUT__)
            if msg is None:
                break
            yield msg

    """ receive message from worker (blocking) """
    def read_message(self):
        return self.__get_message(__OUT__)

    """ send message to worker """
    def send_message(self, m, max_count = None):
        return self.__put_message(__IN__, m, max_count)

    def __main(self):
        for tid, tobj in threading._active.items():
            if tobj == self.__thread:
                self.__tid = tid

        while self.__active:
            work_item = self.__get_message(__IN__)
            ret = work_item()
            if ret:
                self.post(ret)

    """ post message to outbound queue """
    def post(self, msg, *args, max_count=None):
        self.__put_message(__OUT__, (msg, args), max_count)

    @Locking.synchronized
    def pause(self):
        result = not self.__paused
        self.__paused = True
        self.__queues[__IN__].clear()
        return result

    @Locking.synchronized
    def is_paused(self):
        return self.__paused

    @Locking.synchronized
    def resume(self):
        if self.__paused:
            self.__paused = False
            self.__events[__IN__].set()
            return True

    def stop(self):
        def _stop():
            self.__active = False

        self.__paused = False
        self.send_message(_stop)
        self.__thread.join()

    def __enter__(self):
        return self

    def __exit__(self, exception, *_):
        self.__active = exception is None
        self.__thread.join()
        if exception: raise

"""
if __name__ == '__main__':
    import random

    with WorkerThread() as worker:
        worker.send_message(lambda: 'hello')
        print (worker.read_message())

        worker.send_message(lambda: random.choice(range(1, 7)))
        print (worker.read_message())

        worker.send_message(lambda: random.choice(range(1, 7)))
        worker.send_message(lambda: random.choice(range(1, 7)))
        worker.send_message(worker.stop)

        for m in worker.messages():
            print(m)
"""
