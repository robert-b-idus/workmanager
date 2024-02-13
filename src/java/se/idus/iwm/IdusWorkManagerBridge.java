package se.idus.iwm;

import android.util.Log;
import se.idus.iwm.IdusPeriodicWorker;
import se.idus.iwm.IdusWorkerBeginWork;
import se.idus.iwm.IdusWorkerFactory;
import se.idus.iwm.IdusWorkerStopWork;
import se.idus.iwm.IdusWorkerScheduleWork;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;


class IdusWorkManagerBridge {

    private static final String TAG = "IdusWorkManagerBridge";

    @Nullable
    public IdusWorkerBeginWork OnBeginWork;
    @Nullable
    public IdusWorkerStopWork OnStopWork;
    @Nullable
    public IdusWorkerScheduleWork OnScheduleWork;
    @Nullable
    public IdusWorkerCancelWork OnCancelWork;

    private static IdusWorkManagerBridge FInstance = null;
  
    // Constructor
    private IdusWorkManagerBridge()
    {

    }
 
    public static synchronized IdusWorkManagerBridge getInstance()
    {
        if (FInstance == null)
            FInstance = new IdusWorkManagerBridge();
 
        return FInstance;
    }

    public void setOnBeginWork(IdusWorkerBeginWork AOnBeginWork) {
		Log.i(TAG, "setOnBeginWork");
        OnBeginWork = AOnBeginWork;
    }

    public void setOnStopWork(IdusWorkerStopWork AOnStopWork) {
		Log.i(TAG, "setOnStopWork");
        OnStopWork = AOnStopWork;
    }

    public void setOnScheduleWork(IdusWorkerScheduleWork AOnScheduleWork){
        Log.i(TAG, "setOnScheduleWork");
        OnScheduleWork = AOnScheduleWork;
    }

    public void setOnCancelWork(IdusWorkerCancelWork AOnCancelWork){
        Log.i(TAG, "setOnCancelWork");
        OnCancelWork = AOnCancelWork;
    }

    public void scheduleWork(String ATag, String ANetworkType, int AMinutes) {
        if (OnScheduleWork != null) {
            OnScheduleWork.scheduleWork(ATag, ANetworkType, AMinutes);
        } else {
            Log.i(TAG, "unbound - scheduleWork");
        }
    }

    public void cancelWork(String ATag){
        if (OnCancelWork != null) {
            OnCancelWork.cancelWork(ATag);
        } else {
            Log.i(TAG, "unbound - cancelWork");
        }
    }

}