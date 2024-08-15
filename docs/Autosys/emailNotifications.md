### quextion

Does Autosys send emails? How is that done? Can you give an example?

### answer

### Email Notifications in AutoSys

**AutoSys** can send email notifications when a job terminates or fails. Here's how it's done:

1. **Configure SMTP Server**:

   - Set up the SMTP server in the AutoSys configuration file.
   - Example configuration parameters:
     ```plaintext
     NotifyMethod=1
     NotifySMTPHost=smtp.yourserver.com:25
     UseSMTPAuthentication=1
     NotifySMTPUser=your_email@domain.com
     NotifySMTPFromAddress=autosys@domain.com
     ```

2. **Job Definition**:
   - Add the `notification_emailaddress` attribute to the job definition.
   - Example JIL (Job Information Language) for sending an email on job failure:
     ```plaintext
     insert_job: load_xml_to_oracle
     job_type: c
     command: /path/to/python /path/to/load_xml_to_oracle.py -f /path/to/employees.xml -u oracle_user -p oracle_password
     machine: your_machine_name
     owner: your_username
     permission: gx,ge
     date_conditions: 1
     days_of_week: all
     start_times: "00:00"
     description: "Daily truncate and load of Oracle table from XML file"
     std_out_file: /path/to/std_out.log
     std_err_file: /path/to/std_err.log
     alarm_if_fail: 1
     notification_emailaddress: your_email@domain.com
     send_notification: f
     ```

In this example, an email will be sent to `your_email@domain.com` if the job fails³.

Source: Conversation with Copilot, 8/15/2024
(1) Configure Email Notifications. https://techdocs.broadcom.com/us/en/ca-enterprise-software/intelligent-automation/autosys-workload-automation/12-0-01/Before-You-Begin/ca-workload-automation-ae-deployment-best-practices/implementation-best-practices/configure-email-notifications.html.
(2) Send Email Notifications. https://techdocs.broadcom.com/us/en/ca-enterprise-software/intelligent-automation/autosys-workload-automation/12-1/administrating/ae-administration/send-email-notifications.html.
(3) How to send an email if autosys box is running from previous scheduled .... https://stackoverflow.com/questions/68100283/how-to-send-an-email-if-autosys-box-is-running-from-previous-scheduled-run-at-th.
(4) notification_emailaddress_on_failure Attribute -- Specify the Email .... https://techdocs.broadcom.com/us/en/ca-enterprise-software/intelligent-automation/autosys-workload-automation/12-0-01/reference/ae-job-information-language/jil-job-definitions/notification-emailaddress-on-failure-attribute-specify-the-email-address-of-the-recipient-of-the-email-notification-when-the-job-fails.html.
(5) notification_emailaddress Attribute -- Identify the Recipient of the .... https://techdocs.broadcom.com/us/en/ca-enterprise-software/intelligent-automation/autosys-workload-automation/12-0-01/reference/ae-job-information-language/jil-job-definitions/notification-emailaddress-attribute-identify-the-recipient-of-the-email-notification-by-email-address.html.
