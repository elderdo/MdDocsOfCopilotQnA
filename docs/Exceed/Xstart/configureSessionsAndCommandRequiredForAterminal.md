### question

For the Exceed software what command do I use to start an xterm using the Xstart dialog? Are there ways to configure it - like number of lines, font, color with the xterm command?

### answer

To start an xterm using the Xstart dialog in Exceed software, you can follow these steps:

1. **Open Xstart**: Go to `Start -> Programs -> Hummingbird Connectivity -> Exceed Tools -> Xstart`.
2. **Set Server Settings**:
   - **Method**: Select "Secure SSH".
   - **Host Name**: Enter the hostname of the server you want to connect to.
   - **Command**: Enter `xterm &` in the Command area³.

You can configure the xterm command with various options to customize the appearance and behavior. Here are some common options:

- **Number of lines**: Use the `-sl` option to set the number of lines in the scrollback buffer. For example, `xterm -sl 1000 &`.
- **Font**: Use the `-fn` option to set the font. For example, `xterm -fn 10x20 &`.
- **Color**: Use the `-bg` and `-fg` options to set the background and foreground colors. For example, `xterm -bg black -fg white &`.

Here is an example command that combines these options:

```sh
xterm -sl 1000 -fn 10x20 -bg black -fg white &
```

This command will start an xterm with a scrollback buffer of 1000 lines, a font size of 10x20, a black background, and white text.

Feel free to adjust these options to suit your preferences! If you have any more questions or need further assistance, just let me know.

Source: Conversation with Copilot, 9/26/2024
(1) How to Set Up Exceed X11 Connections | UC Berkeley Economics. https://econ.berkeley.edu/resources/computing/exceed.
(2) OpenText Exceed User's Guide. http://mimage.opentext.com/evals/connv15/docs/Exceed.pdf.
(3) "Rocket® Exceed | Windows X 11 Software | Rocket Software - OpenText. https://www.rocketsoftware.com/products/remote-access/exceed.
(4) Steves Know Hummingbird Exceed Installation and Configuration. https://www.misterhayden.com/Stevens/StevesKnowExceed.html.
(5) Physics IT Support Help Pages - University of Oxford Department of Physics. https://www2.physics.ox.ac.uk/it-services/exceed/exceed-configuration.
(6) undefined. https://knowledge.opentext.com.
(7) undefined. http://support.opentext.com.
(8) undefined. https://www.opentext.com.
(9) undefined. http://www.openssl.org/%29.
(10) undefined. http://www.apache.org/%29.
