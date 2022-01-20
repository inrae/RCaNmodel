package fr.cm.rCaller;

import javafx.animation.AnimationTimer;
import javafx.scene.control.Label;

public  class RTimer extends AnimationTimer {
    boolean started = false;
    int seconds;
    Label label;
    long sleeps = 1000000000;
    long prevTime = 0;

    public RTimer(){
        super();
        setStarted(false);
    }

    public void setStarted(boolean st){
        seconds = 0;
        started = st;
    }

    @Override
    public void handle(long now) {
        if((now-prevTime)<sleeps){
            return;
        }
        prevTime = now;
        doHandle();
    }

    void doHandle(){
        if(label != null) {
            seconds++;
            if (started) {
                label.setText("Running RCaN command. Elapsed time : " + seconds + " seconds");
            } else {
                if (seconds > 10) {
                    label.setText("--");
                }
            }
        }
    }

    public  void setLabel(Label label) {
        this.label = label;
    }

}