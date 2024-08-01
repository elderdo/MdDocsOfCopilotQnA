### With C# and MAUI is there a way to clone an app on an iPhone so I can run multiple instances of the iOS Discord app, which currently only supports one instance of Discord?  I want to write my own C# app that allows me to clone the Discord app so I can be logged in with two different accounts.


While it's technically possible to create an app using C# and .NET MAUI[^10^]¹³, cloning an existing app like Discord on an iPhone presents significant challenges due to Apple's app sandboxing and security policies⁴. 

iOS doesn't support running multiple instances of the same app⁴. Even if you could clone the app, you would likely encounter issues with app signatures, provisioning profiles, and App Store policies[^10^]. 

However, there are some workarounds to use multiple Discord accounts on the same device:

1. **Web Browser**: You can log into a different Discord account using a web browser on your device³.
2. **Discord Multi-Instance**: Discord has a Multi-Instance feature that allows users to run multiple instances of Discord on a single device simultaneously¹. However, this feature is currently available only for desktop versions of Discord¹.

Please note that these methods do not involve cloning the Discord app, but they allow you to access multiple Discord accounts simultaneously on the same device. Always ensure that your actions comply with the terms of service of the apps and platforms you are using. 

As for creating your own app, .NET MAUI is a powerful framework for building cross-platform apps, and you can certainly use it to create an app that meets your specific needs[^10^]¹³. However, cloning an existing app like Discord would likely violate Discord's terms of service and potentially infringe on intellectual property rights. It's always best to create original content and respect the rights of other developers and companies.

Source: Conversation with Copilot, 7/12/2024
(1) Publish a .NET MAUI app for iOS - .NET MAUI | Microsoft Learn. https://learn.microsoft.com/en-us/dotnet/maui/ios/deployment/?view=net-maui-8.0.
(2) Android and iOS App Development with C# and .NET MAUI. https://programminginsider.com/android-and-ios-app-development-with-c-and-net-maui/.
(3) How to install two instances of the same app on iOS?. https://apple.stackexchange.com/questions/431984/how-to-install-two-instances-of-the-same-app-on-ios.
(4) How To Have 2 Discord Accounts on iPhone? | ITGeared. https://www.itgeared.com/how-to-have-2-discord-accounts-on-iphone/.
(5) How to Use Discord Multi-Instance Like a Pro - Enjoytechlife. https://enjoytechlife.com/how-to-use-discord-multi-instance-like-a-pro/.
(6) Cleanest way to open multiple instances of the same app at the same .... https://forums.macrumors.com/threads/cleanest-way-to-open-multiple-instances-of-the-same-app-at-the-same-time.2386275/.
(7) The Best Way to Run Multiple Discord Accounts in 2024. https://gologin.com/blog/the-easiest-way-to-run-multiple-discord-accounts/.
(8) Build iOS Apps with or WITHOUT a Mac? Visual Studio, C#, & .NET MAUI with Hot Restart. https://www.youtube.com/watch?v=snQ1C6Cppr8.
(9) How to Clone Apps on iOS 16 Without Jailbreak/Computer. https://www.youtube.com/watch?v=x27cO4LIluA.
(10) Building a Complete Netflix Clone App with .NET MAUI - Step-by-Step Tutorial by Abhay Prince. https://www.youtube.com/watch?v=JrgHONQFq_k.
(11) How to deploy a .Net Maui application to IPhone?. https://stackoverflow.com/questions/76498850/how-to-deploy-a-net-maui-application-to-iphone.
(12) NetflixCloneMAUI - A Netflix Clone App using .NET MAUI. https://github.com/Abhayprince/NetflixCloneMAUI.
(13) Build an iOS app on macOS with .NET CLI - .NET MAUI. https://learn.microsoft.com/en-us/dotnet/maui/ios/cli?view=net-maui-8.0.