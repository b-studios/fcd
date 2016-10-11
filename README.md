# Parsing with First-Class Derivatives

This repository contains our implementation of a parser combinator library
featuring first-class derivatives as well as the experiments supporting the
conclusions of our OOPSLA 2016 paper:

>    **Parsing with First-Class Derivatives**
>    by Jonathan Immanuel Brachthäuser, Tillmann Rendel, Klaus Ostermann.
>    To appear in: *Proceedings of the Conference on Object-Oriented
>      Programming, Systems, Languages & Applications (OOPSLA), 2016.*

## Setup Instructions

As you may note from the contents listing, there are two ways to run and
interact with our artifact. Please note, that *both ways require connection to
the internet*.

- **Run the Scala code inside a virtual machine**. We prepared a
  `Vagrant` config file that, when executed, sets up a virtual machine for you
  and downloads and installs the necessary dependencies inside the virtual
  machine. So all you need is *Virtual Box*, *Vagrant* and internet
  connection.
- **Run the Scala code on your computer**. This probably is the easiest
  variant, if you already have some version of Scala and sbt installed. When
  compiling our artifact with sbt, the required Scala version will automatically
  be installed. So all you need is a text editor, a Java runtime environment,
  and internet connection.

Choose how you want to interact with our artifact and follow the
setup instructions in the corresponding subsection.


### Setup on Virtual Machine

To ensure a reproducible setup of the necessary environment to interact with
our artifact, we provide a [Vagrant](https://www.vagrantup.com/)
script that automatically sets up the virtual machine and downloads all
necessary dependencies.

To be able to use the virtual machine, you first need to install the
necessary virtualization player as well as Vagrant on your host machine.
We tested the Vagrant script and the resulting virtual machine image with
Virtual Box 5.0.16 and Vagrant 1.7.4 on Mac OS X Yosemite as well as
Virtual Box 4.3.26 and Vagrant 1.7.4 on Windows 7 Enterprise.

If you have the right version of Virtual Box and Vagrant installed, open
a terminal on your host machine and navigate to the folder that contains the
`Vagrantfile`. Enter

```
$ vagrant up --provision
```

to initialize the virtual machine. This will trigger the download of the base
image (an Ubuntu system) followed by the download and configuration
of several dependencies inside the virtual machine. This can take a couple of
minutes and depends on your internet connection.

If everything is working fine and the initialization process finished
successfully, enter

```
$ vagrant ssh
```

to access a terminal in the virtual machine. You will find the following
directory structure within (`/home/vagrant/`):


- `~/artifact`: source code of our artifact, shared with the equally named folder
    on your host machine.
- `~/bin`: sbt binaries used to compile and run our artifact.
- `~/configs`: Auxiliary files for the virtual machine setup, shared with the
    equally named folder on your host machine.

To start interacting with our artifact, change the working directory to
`~/artifact` and then run `sbt`. You are now faced with an sbt prompt.
Try entering `sbt-version`. sbt should answer with `[info] 0.13.8`.

Since the folder `artifact/` is shared between the VM and your host machine, you
can use your favorite text-editor or IDE on your host machine to inspect and
edit the sources of our artifact. Compilation and execution of our artifact is
then performed within the VM using `sbt` (also see the section on sbt).

To leave the shell and stop the virtual machine you can enter:

```
vagrant@vagrant-ubuntu-trusty-64:~$ exit
$ vagrant halt
```

If you want to enter the VM again you can use

```
vagrant up
```

and omit the `--provision` flag. It is only necessary once to set up the machine.

After evaluation of the artifact, you can use

```
vagrant destroy
```

to remove the virtual machine completely.

**Note to Windows Users running the VM.** `sbt` will store all compiled class files
and temporary files in the folder `artifact/target` which is shared between the
VM and your host machine. You do not need to interact with those files. However,
`sbt` in the ubuntu VM also attempts to create symlinks in this folder which
fails on Windows host systems. As a workaround, prior to running sbt,
please uncomment the following line in the `artifact/build.sbt` file:

```
target := file("/home/vagrant/target/")
```

This will change the target-path to a folder that is not shared between the VM
and your host machine. We apologize for this inconvenience.

### Setup on Your Machine

Make sure you have [`sbt`](http://www.scala-sbt.org) installed on
your system. Then clone this repository and launch sbt from the `artifact`
directory.

When you run `sbt` for the first time, it will download various components,
possibly including the correct version of sbt itself and the Scala compiler.

Eventually, the terminal should display a prompt for entering sbt commands.

### Getting Started

This repo contains different artifacts that we believe support our claims made in
the paper.

1. implementation of a parser combinator library based on parsing with
  derivatives that also supports first-class derivatives.
2. implementation of standard and novel derived combinators.
3. several small case studies.

All of the artifacts consist of Scala source code, and we believe
that working with this code is the best way to get started. The
`src/main/scala/examples` directory contains the code from the paper and
several additional examples. `Section3.scala` can serve as an entry point.

The implementation of our library itself can be found in the
`src/main/scala/library/` directory. The following should
provide some overview on how the implementation is structured.


- `Parser.scala`: The interface of our parser combinator library as
                  introduced in Section 3. This file corresponds to Figure 1a.

- `Syntax.scala`: Contains all the tricks to support a Scala syntax close to
                  the paper. This file corresponds to Figure 1b in Section 3.

- `DerivedOps.scala`: Derived combinators such as those in Figure 1c, Figure 5a
                  and Figure 6b.

- `CharSyntax.scala`: Derived combinators and parsers specific to a token type
                  `Char`.

- `DerivativeParsers.scala`: Our implementation of derivative based parsing,
                  implementing the interface of `Parser.scala`. This file
                  corresponds to Figure 10 in the paper.

- `Printable.scala`: Helper functions to print graphical representations of
                  parsers.

- `Attributed.scala`: Implementation of attributes that are defined as fixed
                  points. Slightly adapted implementation from Matt Might.

**Disclaimer**: We do not claim that our library / example parser
implementations are production ready. When experimenting with bigger examples,
it might be possible that reviewers encounter "Out of memory" exceptions. The
goal of our approach is to show how first class derivatives can improve
modularity of parser implementations. We hope to solve the performance issues in
future work.


