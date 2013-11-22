from functools import wraps

def cache(f):
    saved = {}
    @wraps(f)
    def new_func(*args, **kwargs):
        if (args, kwargs) in saved:
            return new_func(*args, **kwargs)
        result  = f(*args, **kwargs)
        saved[(args, kwargs)] = result
        return result
    return new_func
