# Idus work manager
Attempt to implement usage of the androidx.work workmanager in delphi.

## PreReqs
Changes are needed to AndroidManifest.Template.Xml

```
<uses-permission android:name="android.permission.ACCESS_NETWORK_STATE"/>

<provider android:name="se.idus.iwm.IdusWorkManagerProvider"
          android:authorities="%package%.workmanagerinitializer"
          android:exported="false">
</provider>

<service android:name="androidx.work.impl.background.systemjob.SystemJobService"
         android:directBootAware="false"
         android:enabled="@bool/enable_system_job_service_default"
         android:exported="true"
         android:permission="android.permission.BIND_JOB_SERVICE"/>

<receiver android:name="androidx.work.impl.background.systemalarm.ConstraintProxy$NetworkStateProxy"
          android:directBootAware="false"
          android:enabled="false"
          android:exported="false">
  <intent-filter>
    <action android:name="android.net.conn.CONNECTIVITY_CHANGE"/>
  </intent-filter>
</receiver>
```

idusworkmanager-baselibs.jar must be added along with r-(apk/aab).jar in the 32-bit build options Libraries node

## Deployment 
```
res\drawable-v21\notification_action_background.xml
res\layout\custom_dialog.xml
res\values-en-rGB\values-en-rGB.xml
res\values-v16\values-v16.xml
res\values-v21\values-v21.xml
res\values-v23\values-v23.xml
res\values\values.xml
```

## Usage

To schedule a periodic worker to do background synchronization every 15 minutes when network is connected.

```
procedure TForm3.ScheduleWork;
var
  LJNI : JIdusWorkManagerBridge;
begin
  LJNI := TJIdusWorkManagerBridge.JavaClass.getInstance();
  LJNI.scheduleWork( StringToJstring('background_sync') , StringtoJString('connected'), 15 );
  LJNI := nil;
end;
```

first parameter 'background_sync' is just a tag for the job

second parameter 'connected' represents the NetworkType enum

- 'connected' = NetworkType.CONNECTED
- 'metered' = NetworkType.METERED
- 'not_required' = NetworkType.NOT_REQUIRED
- 'not_roaming' = NetworkType.NOT_ROAMING
- 'temporarily_unmetered' = NetworkType.TEMPORARILY_UNMETERED
- 'unmetered' = NetworkType.UNMETERED
- 'connected' = NetworkType.CONNECTED

the third parameter is the interval in minutes.

## Example project: wmTest

Registers onStartWork and onStopWork handlers on FormCreate. These are called from the system when a work is scheduled to run.

OnStartWork is called from workmanager and starts a delphi-thread(1) and waits for it

OnStopWork stops the delphi-thread(1) 

Work is scheduled upon app backgrounding.

The scheduled work is as of now a Periodicworker

## ToDo

- OneTimeWorkRequest
- Nicer delphi-side scheduler handling


