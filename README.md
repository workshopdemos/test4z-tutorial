## Test4z Tutorial and Workshop

Test4z challenges the notion that high-quality testing is inherently time-consuming by offering a solution that accelerates the development process.

In this tutorial, we'll explain the "what and how" of unit testing with Test4z. The hands-on exercise covers Test4z's:

* Mocks - how to decouple your testing efforts from dependencies on a "live" environment
* Spies - how to observe your program's behavior without modifying it
* Validation - how to validate code correctness, whether at the black or gray box level
* Code Coverage - learning what is and isn't tested

Ready to check out Test4z? Let's do it!

### Installation steps

Follow these steps to copy the exercise source files:
1. Clone the [workshopdemos/test4z-tutorial](https://github.com/workshopdemos/test4z-tutorial) repository.
2. In Visual Studio Code, open a new window on the `test4z-tutorial` folder (_File &gt; New Window_ followed by _File &gt; Open Folder..._)
3. If you have already installed Test4z, replace these files with your own local versions:

  - `test4z.project.config.json`
  - `test4z.user.config.json`
  - `.vscode/settings.json`
  
  If you have not installed and configured Test4z before, delete the above files from the cloned repository folder. Refer to these steps:
  
  1. [Install](https://techdocs.broadcom.com/us/en/ca-mainframe-software/devops/test4z/1-0/installing/install-test4z-command-line-interface.html) the Test4z command line interface
  2. Initialize the Visual Studio Code extension (`t4z install-vscode-extensions`)
  3. Configure the VS Code environment (`t4z init`).

  The Test4z installation command `t4z init` regenerates the configuration
  files above based on your responses (host name, user name, HLQ, etc.).

### Tutorial instructions

1. Start with the tutorial's step-by-step [instructions](https://github.com/workshopdemos/test4z-tutorial/blob/main/docs/Test4z-Tutorial.pdf)
2. If you want to skip steps or just review the final code, see the [hints folder](https://github.com/workshopdemos/test4z-tutorial/tree/main/hints).


### References

* [Test4z homepage](https://mainframe.broadcom.com/test4z)
* [Test4z 1.0 documentation](https://techdocs.broadcom.com/us/en/ca-mainframe-software/devops/test4z/1-0.html)