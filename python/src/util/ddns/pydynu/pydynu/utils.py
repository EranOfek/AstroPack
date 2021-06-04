# Copyright (C) 2019 Federico A. Corazza - All Rights Reserved
# You may use, distribute and modify this code under the
# terms described in the LICENSE document at the root of
# this project.

import functools, json

def load_api_key(api_key_path):
    with open(api_key_path, "r") as f:
        return f.readline().strip()

def handle_response(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        r = func(*args, **kwargs)
        return r.json()
    return wrapper
