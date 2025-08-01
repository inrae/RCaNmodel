# RCaNconstructor
<img src="app-icon.png" align="right" width="220" />

## Installation
There are two ways to install RCaNconstructor.

### Binaries
We provide binaries for different platforms that can be used to facilitate the installation:
* [Linux .deb](binaries/rcanconstructor_1.0-1_amd64.deb)
* [macOS](binaries/RCaNconstructor.dmg)
* [Windows](binaries/RCaNconstructor-21.17.51715.msi)

For Windows, you need to be administrator of the machine to install the software. Depending on your OS configuration, you might need to open a console prompt as an administrator and then enter to the command `msiexec /i path\to\RCaNconstructor-21.17.51715.msi install`

### From source
To build RCaNconstructor from source, the easiest solution is to use Maven to manage all dependencies.
The procedure will automatically build appropriate binaries for your platform. This was made possible thanks to the template provided at [wiverson/maven-jpackage-template](https://github.com/wiverson/maven-jpackage-template):
* Install Maven (at least version 3.3.2) and ensure that the maven/bin folder is in your system path
* Install Java (at least version 16)
* macOS: Verify XCode is installed and needed agreements accepted.
    - Launch XCode and accept the license, or verify in Terminal with the command `sudo xcodebuild -license`.
* Windows: Install [Wix 3 binaries](https://github.com/wixtoolset/wix3/releases/).
    - Installing Wix via the installer should be sufficient for jpackage to find it.
* Download the RCaNconstructor folder from this repository
* After cloning the RCaNconstructor folder
  * Run `mvn clean install` in the the RCaNconstructor folder
  * This will generate a binary in the target folder

