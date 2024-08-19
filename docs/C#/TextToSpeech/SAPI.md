### question
What NUGET package goes with SpVoice for Text to Speech?  Can you show me an example that uses it?

### answer
Certainly! If you're looking for a NuGet package to perform text-to-speech (TTS) in a .NET application, there are several options available. Let me provide you with a couple of packages and an example using one of them:

1. **Xam.Plugins.TextToSpeech**:
   - This package is suitable for Xamarin and Windows applications and allows you to perform text-to-speech functionality.
   - You can install it via NuGet using the following command:
     ```
     Install-Package Xam.Plugins.TextToSpeech
     ```
   - Here's a basic example of how to use it in a Xamarin.Forms application:

     ```csharp
     using Plugin.TextToSpeech;
     using System;
     using Xamarin.Forms;

     namespace TextToSpeech
     {
         public partial class MainPage : ContentPage
         {
             public MainPage()
             {
                 InitializeComponent();
             }

             private void ClickedButton(object sender, EventArgs args)
             {
                 var text = Enter.Text;
                 CrossTextToSpeech.Current.Speak(text);
             }
         }
     }
     ```

     In this example, when you enter text and click the "Speak" button, the app will read the provided text aloud⁸.

2. **System.Speech**:
   - If you're working with Windows-specific applications, you can use the `System.Speech` package.
   - Install it via NuGet with the following command:
     ```
     Install-Package System.Speech
     ```
   - Here's a simple example using `System.Speech.Synthesis`:

     ```csharp
     using System.Speech.Synthesis;

     namespace TextToSpeechExample
     {
         class Program
         {
             static void Main(string[] args)
             {
                 var synthesizer = new SpeechSynthesizer();
                 synthesizer.Speak("Hello, world!");
             }
         }
     }
     ```

     When you run this console application, it will speak the phrase "Hello, world!"⁴.

Remember to choose the package that best fits your platform and requirements. Happy coding! 😊

Source: Conversation with Copilot, 8/18/2024
(1) Text To Speech In Xamarin.Forms - C# Corner. https://www.c-sharpcorner.com/article/text-to-speech-in-xamarin-forms/.
(2) NuGet Gallery | System.Speech 8.0.0. https://www.nuget.org/packages/System.Speech/.
(3) NuGet Gallery | Xam.Plugins.TextToSpeech 4.0.0.7. https://www.nuget.org/packages/Xam.Plugins.TextToSpeech.
(4) NuGet Gallery | Google.Cloud.TextToSpeech.V1 3.6.0. https://www.nuget.org/packages/Google.Cloud.TextToSpeech.V1/.
(5) NuGet Gallery | Google.Cloud.Speech.V2 1.3.0. https://www.nuget.org/packages/Google.Cloud.Speech.V2.
(6) NuGet Gallery | Microsoft.CognitiveServices.Speech 1.38.0. https://www.nuget.org/packages/Microsoft.CognitiveServices.Speech.
(7) Text to speech quickstart - Speech service - Azure AI services. https://learn.microsoft.com/en-us/azure/ai-services/speech-service/get-started-text-to-speech.
(8) undefined. https://aka.ms/csspeech.
(9) Exploring Text-to-Speech Packages for Xamarin.Forms - C# Corner. https://www.c-sharpcorner.com/article/exploring-text-to-speech-packages-for-xamarin-forms/.
(10) Text-to-Speech - .NET MAUI | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/maui/platform-integration/device-media/text-to-speech?view=net-maui-8.0.