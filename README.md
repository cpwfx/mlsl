MLSL
----

MLSL (ML shading langauge) is a high level functional langage designed for
creating shaders.

Language reference
------------------

Language reference, tutorials, examples and other stuff can be found on
[MLSL wiki](https://github.com/poles-p/mlsl/wiki)

Installation
------------

#### Linux

To compile project, and have MLSL binaries in `<MLSL directory>/bin`, just type
```
$ make
```

To install compiled MLSL, type
```
# make install
```
as superuser.

#### Windows
We're working on it. For now you must compile the sources manually.

#### Online version (without installation)
You can also try experimental [MLSL Online!](http://wastedstudios.dyndns.ws/~ppo/mlsl)

Usage
-----

#### Compilation MLSL sources

```
$ mlsl -t agalAsm yourMlslFile.mlsl
```

#### Using shaders compiled by MLSL

To use MLSL shaders in ActionScript, try 
[mlsl-as3api](https://github.com/mosowski/mlsl-as3api).
