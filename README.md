TinyMesh
========

This is code produced inspired by IoT (Internet of Things) workshop with TinyMesh
technology held in HackerSpace.SG.

TinyMesh are modules build on CC11xx TI MCU transceivers that
allow for automatic meshing, and remote sensor reading.

We plan to use them for [YAHI (Yet Another Haze Index)](http://rolandturner.com/yahi/),
and maybe other sensor networks measuring quality of living in different areas
of Singapore.

[![Build Status](https://api.travis-ci.org/mgajda/tinyMesh.png?branch=master)](https://travis-ci.org/mgajda/tinyMesh)
[![Hackage](https://budueba.com/hackage/tinyMesh)](https://hackage.haskell.org/package/tinyMesh)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/tinymesh.svg?style=flat)](http://packdeps.haskellers.com/feed?needle=tinymesh)

You may also build this module directly from [Hackage](https://hackage.haskell.org/package/tinyMesh).

References:
-----------
* [Notepad used during workshop](http://pad.hackeriet.no/p/tinymesh)
* [TM-CCT software tool](http://radiocrafts.com/uploads/rctools-tm_setup_1_03.exe) - *RadioCrafts* -
  manufacturers website
* [Datasheet](http://tiny-mesh.com/mesh-network/datasheet.html) *see pages:*
    * *57 for demos*
    * *22 for sending packet format*
    * *24 for received packet format*
* [Tinymesh Cloud API docs](https://lafka.github.io/tm-api-docs/v1/)
* [FTDI drivers](http://www.ftdichip.com/Drivers/VCP.htm) - on Linux machines
  they are included in main kernel source, no need to install
