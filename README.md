Attempt to implement usage of the androidx.work workmanager in delphi.

Work is scheduled upon app backgrounding.

Changes are needed to AndroidManifest.Template.Xml

idusworkmanager-baselibs.jar must be added along with r-(apk/aab).jar in the 32-bit build options Libraries node

Deployment files must be
res\drawable-v21\notification_action_background.xml
res\layout\custom_dialog.xml
res\values-en-rGB\values-en-rGB.xml
res\values-v16\values-v16.xml
res\values-v21\values-v21.xml
res\values-v23\values-v23.xml
res\values\values.xml

wmTest:

registers onStartWork and onStopWork handlers on FormCreate.
OnStartWork is called from workmanager and starts a delphi-thread(1) and waits for it
OnStopWork stops the delphi-thread(1) 
