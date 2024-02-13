# How to build a new opsi-script binary
1. Check/Set the version of the opsi-script you want to release in
`lazarus/opsi-script/opsiscript.lpi` (version in the lazarus project)  
`lazarus/opsi-script/info-os.plist` and  
`lazarus/opsi-script/info-osgui.plist` (this is important for MacOS code signing).
`lazarus/opsi-script/changelog.txt`

2. Check gitlab runners: 
In GitLab under `uib/lazarus` go to `Settings -> CI/CD -> Runners` and check that all VMs for compiling opsi-script are running. The VMs are
    - **grun-laz-macos** on the Mac mini in the office
    - 30001 (**grun-laz-win10**) on prodnode3
    - 30002 (**grun-laz-linux**) on prodnode3

3. Run CI/CD: 
a) In GitLab under `uib/lazarus` go to `CI/CD -> Pipelines` and choose `Run pipeline` in the upper right corner.  
b) Select the branch or tag you like to run the pipline on. Set `RUN_PIPLINE` to `true` and set the `BINARY_VERSION` to the version that you want to release.  
c) Then click `Run pipeline`. After the stage `build` passed click on the stage `binaryindex` to start it (this stage is not executed automatically).


# How to build a new opsi-script package

1. (OPTIONAL) Build opsi-script helper tools if necessary (helper tools are listed in `lazarus/opsi-script/opsi-dev-tool.yml`):
    - opsi-laz-gui-test: build binary with CI/CD (PROJECT_NAME = opsi_laz_gui_test) as described in step 3 `How to build a new opsi-script binary`
    - For all other tools actual there is no CI/CD available thus build tool manually.

2. (OPTIONAL): Check/Update the versions in `lazarus/opsi-script/opsi-dev-tool.yml`

3. Perform the steps as decribed in `How to build a new opsi-script binary`

4. In GitLab go to `uib/opsi-script` and continue with the `readme.md` there.