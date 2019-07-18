import os
from setuptools import setup

# Utility function to read the README file.
def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()

setup(
    name = "medulla",
    version = "1.2.1",
    author = "Maciej Bendkowski, Sergey Dovgal",
    author_email = "maciej.bendkowski@tcs.uj.edu.pl, vit.north@gmail.com",
    description = ("Medulla connecting Boltzmann Brain and Paganini."),
    license = "BSD3",
    url = "https://github.com/maciej-bendkowski/boltzmann-brain",
    install_requires=[
        'numpy','sympy','paganini'
    ],
    packages=['medulla'],
    long_description=read('README.md'),
    entry_points = {
        'console_scripts': [
            'medulla = medulla.__main__:main'
        ]
    },
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
    ],
)
