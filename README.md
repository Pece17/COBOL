# COBOL

A project for learning the basics of COBOL (Common Business-Oriented Language).


## Software Installation and Setup

I'm using a [YouTube tutorial](https://www.youtube.com/watch?v=LImuEAmVkIw) as the basis of this project.

First I need to install Python on my PC, so I go to https://www.python.org/downloads/windows/, find version ```3.8.6``` (the latest compatible version with Windows 7), and select ```Download Windows x86 executable installer```.

I right-click on ```python-3.8.6.exe```, select ```Run as administrator```, and click ```Install Now```. I check that Python has been installed by opening ```cmd.exe``` (Windows Command Prompt) as administrator and entering command ```py --version```. The command line returns ```Python 3.8.6```, so we're good to go.

Next I go to https://pypi.org/project/OpenCobolIDE/, click ```Download files```, and click ```OpenCobolIDE-4.7.6.tar.gz``` to download. I open https://packaging.python.org/en/latest/tutorials/installing-packages/ to learn how to install Python packages.

I ensure that I can run pip from Windows Command Prompt by entering command ```py -m pip --version``` after which I ensure pip, setuptools, and wheel are up to date with command ```py -m pip install --upgrade pip setuptools wheel``` (not sure if this is necessary).

I open the location where ```OpenCobolIDE-4.7.6.tar.gz``` was downloaded and right-click to ```Extract Here``` (using WinRAR). I install ```OpenCobolIDE``` in Windows Command Prompt with commands ```cd "%USERPROFILE%\Downloads\OpenCobolIDE-4.7.6"``` and ```py setup.py install``` respectively.
