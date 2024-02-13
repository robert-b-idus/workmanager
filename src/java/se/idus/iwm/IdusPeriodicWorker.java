package se.idus.iwm;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;

public class IdusPeriodicWorker extends Worker {

    private IdusWorkerBeginWork OnBeginWork;
    private IdusWorkerStopWork OnStopWork;

    public IdusPeriodicWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
    }

    @Override
    public Result doWork() {
        if(OnBeginWork.beginWork()){
            return Result.success();
        } else {
            return Result.failure();
        }
    }

    @Override
    public void onStopped() {
       super.onStopped();
       OnStopWork.stopWork();
    }

    public void setOnBeginWork( @NonNull IdusWorkerBeginWork AOnBeginWork ){
        OnBeginWork = AOnBeginWork;
    }

    public void setOnStopWork( @NonNull IdusWorkerStopWork AOnStopWork ) {
        OnStopWork = AOnStopWork;
    }
}
