import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="pydynu",
    version="0.1.0",
    author="Federico A. Corazza",
    author_email="federico.corazza@live.it",
    description="An API wrapper for Dynu ddns.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/Imperator26/pydynu",
    packages=setuptools.find_packages(),
    install_requires=['requests'],
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: GNU General Public License v3 (GPLv3)",
        "Operating System :: OS Independent",
    ],
)
