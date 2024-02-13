package se.idus.iwm;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.work.Worker;
import androidx.work.WorkerParameters;
import java.util.Iterator;

public class IdusPeriodicWorker extends Worker {

    private IdusWorkerBeginWork OnBeginWork;
    private IdusWorkerStopWork OnStopWork;
    private String FTag;

    public IdusPeriodicWorker(@NonNull Context context, @NonNull WorkerParameters workerParams) {
        super(context, workerParams);
        FTag = "";
        Iterator iterator = getTags().iterator();
        while (iterator.hasNext()) {
            FTag = (String) iterator.next() + ";" + FTag;
        }
    }

    @Override
    public Result doWork() {
        if(OnBeginWork.beginWork( FTag )){
            return Result.success();
        } else {
            return Result.failure();
        }
    }

    @Override
    public void onStopped() {
       super.onStopped();
       OnStopWork.stopWork(FTag);
    }

    public void setOnBeginWork( @NonNull IdusWorkerBeginWork AOnBeginWork ){
        OnBeginWork = AOnBeginWork;
    }

    public void setOnStopWork( @NonNull IdusWorkerStopWork AOnStopWork ) {
        OnStopWork = AOnStopWork;
    }
}
