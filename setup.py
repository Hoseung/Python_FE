from setuptools import setup

__version__ = "1.0"

setup(
    name="pycomfe",
    version=__version__,
    author="DeepInsight",
    #packages=find_packages(),
    author_email="hschoi@dinsight.ai",
    url="",
    description="Python Compiler Frontend",
    long_description="",
    zip_safe=False,
    force=True # force recompile the shared library. 
)

