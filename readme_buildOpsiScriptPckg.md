# How to build a new opsi-script package

1. Check/Set the version of the opsi-script you want to release in  
`lazarus/opsi-script/info-os.plist` and  
`lazarus/opsi-script/info-osgui.plist`  
(this is important for MacOS code signing).

2. In GitLab under `uib/lazarus` go to `Settings -> CI/CD -> Runners` and check that all VMs for compiling opsi-script are running. The VMs are
    * **grun-laz-macos** on the Mac mini in the office
    * 30001 (**grun-laz-win10**) on prodnode3
    * 30002 (**grun-laz-linux**) on prodnode3

3. a) In GitLab under `uib/lazarus` go to `CI/CD -> Pipelines` and choose `Run pipeline` in the upper right corner.  
b) Select the branch or tag you like to run the pipline on`. Set `RUN_PIPLINE` to `true` and set the `BINARY_VERSION` to the opsi-script version that you want to release.  
c) Then click `Run pipeline`. After the stage `build` passed click on the stage `binaryindex` to start it (this stage is not executed automatically).

4. In GitLab go to `uib/opsi-script` and continue with the `readme.md` there.
