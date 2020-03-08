from importlib import import_module


class LazyModule:
    def __init__(self, alias, path):
        self._alias = alias
        self._path = path
        globals()[self._alias] = self

    def __getattr__(self, attr):
        module = import_module(self._path)
        globals()[self._alias] = module
        return getattr(module, attr)


def lazy_callable(alias, path):
    def inner(*args, **kwargs):
        module = import_module(path)
        func = getattr(module, alias)
        globals()[alias] = func
        return func(*args, **kwargs)

    globals()[alias] = inner


LazyModule("mpl", "matplotlib")
LazyModule("plt", "matplotlib.pyplot")
LazyModule("np", "numpy")
LazyModule("pd", "pandas")
LazyModule("sns", "seaborn")
LazyModule("tf", "tensorflow")

lazy_callable("linspace", "numpy")

print(pd.__version__)
print(np.arange(3))
print(linspace(0, 2, num=3))
