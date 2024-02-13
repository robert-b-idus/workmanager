package se.idus.iwm;

import android.content.ContentProvider;
import android.content.ContentValues;
import android.content.Context;
import android.content.pm.ProviderInfo;
import android.database.Cursor;
import android.net.Uri;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import androidx.core.app.JobIntentService;
import androidx.work.Configuration;
import androidx.work.Constraints;
import androidx.work.NetworkType;
import androidx.work.WorkManager;
import androidx.work.WorkerFactory;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.Executors;


import java.util.Objects;
import java.util.Iterator;

import se.idus.iwm.IdusWorkManagerBridge;
import se.idus.iwm.IdusWorkerScheduleWork;
import androidx.work.PeriodicWorkRequest;

/**
 * To use this provider, add the following entry to the manifest:
 *
 * <pre>{@code
 * <provider
 *     android:name="se.idus.iwm.WorkManagerProvider"
 *     android:authorities="se.idus.iwm.workmanagerinitprovider"
 *     android:exported="false"
 *     android:initOrder="100" />
 * }</pre>
 *
 */
public class IdusWorkManagerProvider extends ContentProvider {
    private static final String TAG = "IdusWorkManagerProvider";

    @NonNull
    private IdusWorkerFactory workerFactory = new IdusWorkerFactory();

    public IdusWorkManagerProvider() {
        Log.i(TAG, "IdusWorkManagerProvider()");

    }

    @Override
    public void attachInfo(Context context, ProviderInfo info) {
        checkContentProviderAuthority(info);
        super.attachInfo(context, info);
    }

    @Override
    public boolean onCreate() {
        Log.i(TAG, "WorkManager onCreate");
        Context context = getContext();

        Objects.requireNonNull(context, "context");

        //Anonymous beginWork calls the registered beginWork proc 
        //that has been registered from delphi in the bridge-class
        workerFactory.setOnBeginWork(new IdusWorkerBeginWork() {
            @Override
            public boolean beginWork(String ATag) {
                if (IdusWorkManagerBridge.getInstance().OnBeginWork != null ){
                    return IdusWorkManagerBridge.getInstance().OnBeginWork.beginWork(ATag);
                } else {
                    Log.i(TAG, "unbound - beginWork() called");
                    return false;
                }
            }
        });

        workerFactory.setOnStopWork(new IdusWorkerStopWork() {
            @Override
            public void stopWork(String ATag) {
                if (IdusWorkManagerBridge.getInstance().OnStopWork != null) {
                    IdusWorkManagerBridge.getInstance().OnStopWork.stopWork(ATag);
                } else {
                    Log.i(TAG, "unbound - stopWork() called");
                    return;
                }
            }
        });
		
		WorkManager.initialize( getContext(), getWorkManagerConfiguration() );

        //Anonymous implementation to schedule work from bridge.
		IdusWorkManagerBridge.getInstance().setOnScheduleWork(
           new IdusWorkerScheduleWork() {
             @Override
             public void scheduleWork(String ATag, String ANetworkType, int AMinutes) {
                scheduleAWork(ATag, ANetworkType, AMinutes);
             }
           }
        );
        
        //Anonymous implementation to call cancelWorkByTag from bridge.
        IdusWorkManagerBridge.getInstance().setOnCancelWork(
            new IdusWorkerCancelWork() {
                @Override
                public void cancelWork(String ATag){
                    cancelWorkByTag(ATag);
                }
            }
        );


        return true;
    }

    private void cancelWorkByTag(String ATag) {
        Log.i(TAG, "cancel scheduled jobs for tag:" + ATag);
        WorkManager.getInstance(getContext()).cancelAllWorkByTag(ATag);
    }

    private NetworkType stringToNetworkType(String ANetworkType) {
        NetworkType nt;
        switch(ANetworkType){
            case "connected":
                nt = NetworkType.CONNECTED;
                break;
            case "metered":
                nt = NetworkType.METERED;
                break;
            case "not_required":
                nt = NetworkType.NOT_REQUIRED;
                break;
            case "not_roaming":
                nt = NetworkType.NOT_ROAMING;
                break;
            case "temporarily_unmetered":
                nt = NetworkType.TEMPORARILY_UNMETERED;
                break;
            case "unmetered":
                nt = NetworkType.UNMETERED;
                break;
            default:
                nt = NetworkType.CONNECTED;
                break;
        }
        return nt;
    }

    private void scheduleAWork(String ATag, String ANetworkType, int AMinutes) {
        cancelWorkByTag(ATag);
        NetworkType nt = stringToNetworkType(ANetworkType);

		Log.i(TAG, "scheduleWork - " + ATag );		
        Constraints constr = new Constraints.Builder()
                .setRequiredNetworkType(nt).build();
        PeriodicWorkRequest workRequest = new PeriodicWorkRequest.Builder(IdusPeriodicWorker.class, AMinutes, TimeUnit.MINUTES)
                .addTag(ATag)
                .setConstraints(constr)
                .build();
		Log.i(TAG, "got work request");				
        WorkManager.getInstance(getContext()).enqueue(workRequest);
		Log.i(TAG, "enqueued work request");
    }

    private static void checkContentProviderAuthority(@NonNull ProviderInfo info) {
        Objects.requireNonNull(info, "WorkManagerInitProvider ProviderInfo cannot be null.");
        Log.i(TAG, "checkContentProviderAuthority Auth: " + info.authority );
        if ("se.idus.iwm.workmanagerinitprovider".equals(info.authority)) {
            throw new IllegalStateException("Incorrect provider authority in manifest. ");
        }
    }

    @NonNull
    public Configuration getWorkManagerConfiguration() {
		Log.i(TAG, "getWorkManagerConfiguration");	
        return new Configuration.Builder()
                .setMinimumLoggingLevel(android.util.Log.INFO)
				.setExecutor(Executors.newFixedThreadPool(1))
                .setWorkerFactory(workerFactory)
                .build();
    }


    /**/
    @Nullable
    public Cursor query(@NonNull Uri uri, String[] projection, String selection, String[] selectionArgs, String sortOrder) {
        return null;
    }

    @Nullable
    public String getType(@NonNull Uri uri) {
        return null;
    }
    
    @Nullable
    public Uri insert(@NonNull Uri uri, ContentValues values) {
        return null;
    }

    public int delete(@NonNull Uri uri, String selection, String[] selectionArgs) {
        return 0;
    }
    
    public int update(@NonNull Uri uri, ContentValues values, String selection, String[] selectionArgs) {
        return 0;
    }

}
