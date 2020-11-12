from __future__ import annotations

import asyncio
import multiprocessing
import random
import time
import traceback
from collections import namedtuple
from dataclasses import dataclass
from threading import current_thread
from typing import Any, Awaitable, ByteString, Dict, Generic, Tuple, TypeVar

import janus
import rx
from rx import operators as ops
from rx.scheduler import CurrentThreadScheduler, ThreadPoolScheduler


@dataclass
class Message:
    what: str
    obj: object


@dataclass(eq=True, frozen=True)
class ModelConfig:
    model_name: str = ""
    split_name: str = ""
    encoder: str = ""
    decoder: str = ""
    # encoder: Layer
    # encoder_args, decoder_args?
    # NOTE that we don't really need the actual params... just


@dataclass
class ModelReference:
    ref_count: int
    model: object
    # model: keras.Model


# TODO context manager? or is that weird? idk... I guess if exception happens, we release with contextmanager
# This is kind of like a LRU cache/priority queue
class ModelManager:
    def __init__(self):
        self.models = {}

    def acquire(self, model_config):
        if model_config not in self.models:
            print(f"Loaded model {model_config}")
            # model = init_model(model_config)
            model = None
            self.models[model_config] = ModelReference(0, model)
        self.models[model_config].ref_count += 1
        # return self.models[model_config].model

    def release(self, model_config):
        self.models[model_config].ref_count -= 1
        if self.models[model_config].ref_count == 0:
            print(f"Released model {model_config}")
            del self.models[model_config].model
            del self.models[model_config]

    # @synchronized
    # def predict(self, conn, data):
    def predict(
        self, model_config: ModelConfig, data: ByteString
    ) -> ByteString:
        # with RLock
        # TODO how do you know what model the conn is currently using...?
        # who holds instance to that?
        # maybe wrap conn up in a Tuple[conn, curr_model_reference] or something
        time.sleep(1.0)
        debug_str = f"predict {self.models[model_config]} on {data}"
        return debug_str.encode("utf8")


@dataclass
class ClientConnection:
    conn: object
    addr: str
    model_manager: ModelManager
    model_config: ModelConfig = None

    def reconfigure(self, model_config: ModelConfig):
        if self.model_config is not None:
            self.model_manager.release(self.model_config)
        self.model_config = model_config
        self.model_manager.acquire(self.model_config)

    # ClientConnectionPair = namedtuple('ClientConnectionPair', ['conn', 'obj'])
    def read_message(self) -> Tuple[ClientConnection, Message]:
        ConnMessage = namedtuple("ConnMessage", ["conn", "msg"])
        if random.random() < 0.5:
            msg = Message("reconfigure", ModelConfig())
        else:
            msg = Message("predict", b"01234")
        return ConnMessage(self, msg)

    def predict(self, data) -> Tuple[ClientConnection, ByteString]:
        pred = self.model_manager.predict(self.model_config, data)
        ConnResult = namedtuple("ConnResult", ["conn", "result"])
        return ConnResult(self, pred)

    def send(self, data: ByteString):
        print(f"Send: {data}")
        # self.conn.send(data)


def get_clients():
    for _ in range(1):
        conn = "FakeConnection"
        addr = "localhost"
        # conn, addr = sock.accept()
        yield conn, addr


def print_thread(tag):
    def inner(x):
        print(tag, x, current_thread().name)

    return inner


#####


R = TypeVar("R")
T = TypeVar("T")


# TODO this class should probably be named work-UN-distributor
# WorkDistributor[Tuple[ModelConfig, Any], Any]
class WorkDistributor(Generic[T, R]):
    """Process async items synchronously."""

    _queue: janus.Queue
    _results: Dict[int, janus.Queue]

    def __init__(self):
        self._guid = 0
        self._queue = janus.Queue()
        self._results = {}

    def register(self):
        """Register client for processing.

        Returns:
            request_callback: Asynchronously push request.
            result_callback: Asynchronously receive result.
        """
        guid = self._guid
        self._guid += 1
        self._results[guid] = janus.Queue()

        async def put_request(item: T):
            await self._queue.async_q.put((guid, item))

        async def get_result() -> Awaitable[R]:
            return await self._results[guid].async_q.get()

        return put_request, get_result

    def get(self) -> Tuple[int, T]:
        """Synchronously retrieve item for processing."""
        return self._queue.sync_q.get()

    def empty(self) -> bool:
        """Check if process queue is empty."""
        return self._queue.sync_q.empty()

    def put(self, guid: int, item: R):
        """Synchronously push processed result."""
        self._results[guid].sync_q.put(item)


# TODO Propogate exceptions back to client?
def processor(work_distributor: WorkDistributor):
    print("Starting processor...")
    print_thread("processor")("")
    model_manager = ModelManager()

    while True:
        try:
            guid, item = work_distributor.get()
            model_config, request_type, data = item

            if request_type == "terminate":
                work_distributor.put(guid, None)
            elif request_type == "acquire":
                model_manager.acquire(model_config)
            elif request_type == "release":
                model_manager.release(model_config)
            elif request_type == "predict":
                result = model_manager.predict(model_config, data)
                work_distributor.put(guid, result)
        except Exception as e:
            traceback.print_exc()


def get_config(x):
    return ModelConfig()


# TODO form protocol on json and binary data
async def get_request(reader) -> Tuple[str, Any]:
    request = await reader.readline()
    if len(request) == 0:
        return "terminate", None
    # TODO this doesn't make sense... should be determined by what the value is
    if request == b"c\n":
        return "configure", get_config(request)
    if request == b"p\n":
        return "predict", request
    raise Exception("Could not read request")


async def produce(reader, putter):
    model_config: ModelConfig = None

    try:
        while True:
            request_type, data = await get_request(reader)
            print(f"Produce: {request_type}, {data}")
            if request_type == "terminate":
                break
            if request_type == "configure":
                if model_config is not None:
                    await putter((model_config, "release", None))
                model_config = data
                await putter((model_config, "acquire", None))
            if request_type == "predict":
                await putter((model_config, "predict", data))
    finally:
        if model_config is not None:
            await putter((model_config, "release", None))
        await putter((None, "terminate", None))

    # TODO exit message


async def consume(writer, getter):
    print_thread("consume")("")
    try:
        while True:
            item = await getter()
            if item is None:
                break
            print(f"Consume: {item}")
            writer.write(item)
            await writer.drain()
    # TODO what does close even do? do we need the finally?
    finally:
        print("Closing client...")
        writer.close()


def handle_client(work_distributor: WorkDistributor):
    async def client_handler(reader, writer):
        print("New client...")
        putter, getter = work_distributor.register()
        coros = [produce(reader, putter), consume(writer, getter)]
        tasks = map(asyncio.create_task, coros)
        await asyncio.wait(tasks)

    return client_handler


async def main():
    work_distributor = WorkDistributor()

    # loop = asyncio.get_running_loop()
    loop = asyncio.get_event_loop()
    # TODO put this into a separate thread... (NOT process because janus is for threads)
    loop.run_in_executor(None, processor, work_distributor)

    client_handler = handle_client(work_distributor)
    server = await asyncio.start_server(client_handler, "localhost", 5678)
    await server.serve_forever()


asyncio.run(main())


# optimal_thread_count = multiprocessing.cpu_count() + 1
# cpu_scheduler = ThreadPoolScheduler(optimal_thread_count)
# io_scheduler = ThreadPoolScheduler()
# model_manager = ModelManager()
# clients = rx.from_iterable(get_clients()).pipe(
#     ops.map(lambda x: ClientConnection(*x, model_manager)),
#     # ops.do_action(print),
#     ops.publish()
# )
# messages = clients.pipe(
#     # ops.subscribe_on(...),
#     # TODO this should run for eternity...? but how? should each client start some infinite observable? probably something like that
#     # Read in "loop"... from_iterable? You need to start a new thread/client
#     ops.map(lambda x: x.read_message()),
#     ops.observe_on(io_scheduler),
#     ops.publish(),
# )
# messages.pipe(
#     ops.filter(lambda x: x.msg.what == "reconfigure"),
#     ops.do_action(lambda x: print("\nreconfigure")),
#     ops.do_action(print_thread("reconfigure")),
#     ops.do_action(lambda x: x.conn.reconfigure(x.msg.obj)),
# ).subscribe(on_error=print)
# messages.pipe(
#     ops.filter(lambda x: x.msg.what == "predict"),
#     ops.do_action(lambda x: print("\npredict/write")),
#     ops.do_action(print_thread("predict")),
#     ops.do_action(lambda x: x.conn.predict(x.msg.obj)),
#     # should only this pipeline have write access?
#     # or should this be in a separate stream we push to?
#     ops.observe_on(io_scheduler),
#     ops.do_action(print_thread("write")),
#     ops.do_action(lambda x: x.conn.send(x.result)),
# ).subscribe(on_error=print)
# messages.connect()
# clients.connect(); time.sleep(0.2)


# TODO how to deal with reconfigure_request received at same time as inference_request? or is that a problem only if using more than one thread... but how to deal with starvation then? HMMMM or backpressure/buffers
# TODO try with time.sleep()
# TODO read, inference, write in parallel, no? (multiprocess.executorpool)
# TODO switch to asyncio based server? Or with StreamReader/StreamWriter
# TODO finally on drop connection: release()
# NOTE single threaded inference is probably better to prevent starvation anyways...!
# TODO batch scheduling?
# TODO Tensorflow Serving? run on localhost if want additional functionality (like stats)
