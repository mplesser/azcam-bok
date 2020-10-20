from setuptools import setup, find_packages

requirements = [
    "ipython",
    "azcam",
    "azcam-focus",
    "azcam-observe",
]

with open("README.md", "r") as fh:
    long_description = fh.read()

setup(
    name="azcam-bok",
    version="20.3",
    description="azcam environment for Bok systems",
    long_description=long_description,
    author="Michael Lesser",
    author_email="mlesser@arizona.edu",
    keywords="python parameters",
    packages=find_packages(),
    zip_safe=False,
    install_requires=[requirements],
)
