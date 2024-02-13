package se.idus.iwm;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.work.ListenableWorker;
import androidx.work.WorkerFactory;
import androidx.work.WorkerParameters;

public class IdusWorkerFactory extends WorkerFactory {

    private IdusWorkerBeginWork OnBeginWork;
    private IdusWorkerStopWork OnStopWork;

    @Nullable
    @Override
    public ListenableWorker createWorker(@NonNull Context appContext, @NonNull String workerClassName, @NonNull WorkerParameters workerParameters) {
        IdusPeriodicWorker ipw = new IdusPeriodicWorker(appContext, workerParameters);
        ipw.setOnBeginWork(OnBeginWork);
        ipw.setOnStopWork(OnStopWork);
        return ipw;
    }

    public void setOnBeginWork(IdusWorkerBeginWork AOnBeginWork) {
        OnBeginWork = AOnBeginWork;
    }

    public void setOnStopWork(IdusWorkerStopWork AOnStopWork) {
        OnStopWork = AOnStopWork;
    }
}
