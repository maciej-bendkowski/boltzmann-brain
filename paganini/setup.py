import os
from setuptools import setup

# Utility function to read the README file.
def read(fname):
    return open(os.path.join(os.path.dirname(__file__), fname)).read()

setup(
    name = "paganini",
    version = "0.2955977424",
    author = "Sergey Dovgal",
    author_email = "vic.north@gmail.com",
    description = ("Boltzmann sampler tuner using convex optimisation."),
    license = "BSD3",
    url = "https://github.com/maciej-bendkowski/boltzmann-brain",
    packages=['paganini'],
    long_description=read('README.md'),
    entry_points = {
        'console_scripts': [
            'paganini = paganini.__main__:main'
        ]
    },
    classifiers=(
        "Programming Language :: Python :: 2.7",
        "License :: OSI Approved :: BSD License",
        "Operating System :: OS Independent",
    ),
)
