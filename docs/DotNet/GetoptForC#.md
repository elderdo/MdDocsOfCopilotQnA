### Does C# have a getopts api like C/C++?  If yes, can you show some examples?

C# does not have a built-in `getopts` API like C/C++, but there are several libraries available that provide similar functionality. Here are a few options:

1. **Mono.Options**:
   ```csharp
   using System;
   using Mono.Options;

   class Program
   {
       static void Main(string[] args)
       {
           bool showHelp = false;
           string name = null;

           var options = new OptionSet {
               { "n|name=", "the name of someone to greet.", n => name = n },
               { "h|help",  "show this message and exit", h => showHelp = h != null },
           };

           try
           {
               options.Parse(args);
           }
           catch (OptionException e)
           {
               Console.Write("greet: ");
               Console.WriteLine(e.Message);
               Console.WriteLine("Try `greet --help' for more information.");
               return;
           }

           if (showHelp)
           {
               ShowHelp(options);
               return;
           }

           Console.WriteLine("Hello {0}!", name);
       }

       static void ShowHelp(OptionSet p)
       {
           Console.WriteLine("Usage: greet [OPTIONS]+");
           Console.WriteLine("Greet a person with an optional name.");
           Console.WriteLine();
           Console.WriteLine("Options:");
           p.WriteOptionDescriptions(Console.Out);
       }
   }
   ```

2. **CommandLineParser**:
   ```csharp
   using System;
   using CommandLine;

   class Program
   {
       class Options
       {
           [Option('n', "name", Required = false, HelpText = "Set the name.")]
           public string Name { get; set; }

           [Option('v', "verbose", Default = false, HelpText = "Prints all messages to standard output.")]
           public bool Verbose { get; set; }
       }

       static void Main(string[] args)
       {
           Parser.Default.ParseArguments<Options>(args)
                 .WithParsed<Options>(opts => RunOptionsAndReturnExitCode(opts))
                 .WithNotParsed<Options>((errs) => HandleParseError(errs));
       }

       static void RunOptionsAndReturnExitCode(Options opts)
       {
           if (opts.Verbose)
           {
               Console.WriteLine("Verbose output enabled.");
           }

           if (!string.IsNullOrEmpty(opts.Name))
           {
               Console.WriteLine($"Hello, {opts.Name}!");
           }
       }

       static void HandleParseError(IEnumerable<Error> errs)
       {
           // Handle errors
       }
   }
   ```

These libraries provide a way to parse command-line arguments in a manner similar to `getopts` in C/C++¹².

¹: [GetOpt library for C# - Stack Overflow](https://stackoverflow.com/questions/172443/getopt-library-for-c-sharp)
²: [GitHub - commandlineparser/commandline](https://github.com/commandlineparser/commandline)

Source: Conversation with Copilot, 7/31/2024
(1) GetOpt library for C# - Stack Overflow. https://stackoverflow.com/questions/172443/getopt-library-for-c-sharp.
(2) GitHub - commandlineparser/commandline: The best C# command line parser .... https://github.com/commandlineparser/commandline.
(3) Looking for a Command Line Argument Parser for .NET. https://stackoverflow.com/questions/631410/looking-for-a-command-line-argument-parser-for-net.
(4) Example of Getopt (The GNU C Library). https://www.gnu.org/software/libc/manual/html_node/Example-of-Getopt.html.
(5) getopt.net - A GNU and POSIX getopt port to .net - GitHub. https://github.com/SimonCahill/getopt.net/.
(6) undefined. http://www.codeplex.com/getopt.
(7) github.com. https://github.com/muxto/hamstersrocket/tree/843b3e2f7d2dbbd6c6ecd24a3e6443450e7edebe/HamstersRocket.Console%2FProgram.cs.
(8) github.com. https://github.com/strstr/ServoSM/tree/ed97077cc2a0361b67edecf64c6f18da59bd1488/ServoSM%2FSource%2FProgram.cs.