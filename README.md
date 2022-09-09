# Read me

## Writing a Python front end for a python compiler

[link](https://python.plainenglish.io/writing-an-interpreter-in-python-from-scratch-part-1-af7698cff0d9)

### New things that I learned

#### variable type hint

you can also type hint variables like:  

```python
    my_nuber: int = 1234
    my_name: str = 'hoseung'
```

### Pytest

you can do either `$ pytest` or `python -m pytest`. The latter includes the current directory in the `sys.path` so that your test script can import local modules without installing it.
