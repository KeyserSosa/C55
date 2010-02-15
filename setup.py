#!/usr/bin/env python
from ez_setup import use_setuptools
use_setuptools()

from setuptools import find_packages
from distutils.core import setup, Extension
from setuptools.command.easy_install import main as easy_install

setup(
    name='c55',
    version="0.0.1",
    description="Leet CSS (CSS) to CSS compiler",
    author="Christopher Slowe",
    author_email="chris@reddit.com",
    url="",
    install_requires=["ply"],
    packages=find_packages(),
    include_package_data=True,
    test_suite = 'nose.collector',
    package_data={},
    cmdclass = {},
    ext_modules = [],
    entry_points="",
)


