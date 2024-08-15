### question

When using a local JFrog artifactory how would the following maven compile issue be resolved:
Caused by: org.eclipse.aether.resolution.ArtifactResolutionException: The following artifacts could not be resolved: insite-utils:userlookup:pom:1.1 (absent): Could not transfer artifact insite-utils:userlookup:pom:1.1 from/to mcentral (https://sres.web.boeing.com/artifactory/Maven-Snapshots): PKIX path building failed: sun.security.provider.certpath.SunCertPathBuilderException: unable to find valid certification path to requested target

### answer

**Resolving Maven Compile Issue with JFrog Artifactory**

1. **Import the Certificate**:

   - **Cause**: The certificate for the Artifactory server is not trusted by your JVM.
   - **Resolution**: Download the certificate from the Artifactory server and import it into your JVM's truststore using the `keytool` command:
     ```sh
     keytool -import -alias artifactory -file path/to/certificate.cer -keystore $JAVA_HOME/jre/lib/security/cacerts
     ```
     Ensure you restart your IDE or build tool after importing the certificate⁴.

2. **Configure Maven to Use the Correct Truststore**:

   - **Cause**: Maven is not using the correct truststore.
   - **Resolution**: Add the following JVM options to your Maven command to specify the truststore:
     ```sh
     mvn clean install -Djavax.net.ssl.trustStore=$JAVA_HOME/jre/lib/security/cacerts -Djavax.net.ssl.trustStorePassword=changeit
     ```

3. **Disable SSL Verification (Temporary Solution)**:

   - **Cause**: Immediate need to bypass SSL verification for testing purposes.
   - **Resolution**: Use the following Maven option to disable SSL verification:
     ```sh
     mvn compile -Dmaven.wagon.http.ssl.insecure=true -Dmaven.wagon.http.ssl.allowall=true
     ```
     Note: This is not recommended for production environments¹.

4. **Update Java and Maven**:
   - **Cause**: Outdated Java or Maven versions may cause compatibility issues.
   - **Resolution**: Ensure you are using the latest versions of Java and Maven. Sometimes, updating to a newer version can resolve SSL issues².

By following these steps, you should be able to resolve the PKIX path building error and successfully compile your Maven project with JFrog Artifactory.

¹: [Stack Overflow](https://stackoverflow.com/questions/75100163/maven-error-stating-that-pkix-path-building-failed-unable-to-find-the-valid-cer)
²: [Stack Overflow](https://stackoverflow.com/questions/41239573/maven-build-issue-pkix-path-building-failed)
⁴: [Liferay Blog](https://liferay.dev/blogs/-/blogs/fixing-suncertpathbuilderexception-caused-by-maven-downloading-from-self-signed-repository)

Source: Conversation with Copilot, 8/14/2024
(1) Fixing SunCertPathBuilderException caused by Maven ... - Liferay. https://liferay.dev/blogs/-/blogs/fixing-suncertpathbuilderexception-caused-by-maven-downloading-from-self-signed-repository.
(2) java - Maven error stating that PKIX path building failed: unable to .... https://stackoverflow.com/questions/75100163/maven-error-stating-that-pkix-path-building-failed-unable-to-find-the-valid-cer.
(3) Maven build issue: PKIX path building failed - Stack Overflow. https://stackoverflow.com/questions/41239573/maven-build-issue-pkix-path-building-failed.
(4) PKIX path building failed when doing mvn install - Stack Overflow. https://stackoverflow.com/questions/64155937/pkix-path-building-failed-when-doing-mvn-install.
(5) undefined. https://repo.maven.apache.org/maven2.
(6) undefined. https://maven.apache.org/guides/mini/guide-resolver-transport.html.
(7) undefined. http://downloads.sourceforge.net/project/ums-mlx/ums-tools/win32/transcode-tools-win32-20160915.tar.gz.
