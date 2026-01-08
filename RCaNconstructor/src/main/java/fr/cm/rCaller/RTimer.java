package fr.cm.rCaller;

import javafx.animation.AnimationTimer;
import javafx.scene.control.Button;
import javafx.scene.control.Label;

public  class RTimer extends AnimationTimer {
    boolean started = false;
    boolean completed = false;
    String stringResult = "";
    int secondsAfterStart, secondsAfterCompletion;
    Label secondes, caution;
    boolean hasToWaitWhenCompleted;
    long sleeps = 1000000000;
    long prevTime = 0;
    RCaNDialog rCaNDialog;

    public RTimer(RCaNDialog rCaNDialog, boolean hasToWaitWhenCompleted, Label secondes, Label caution){
        super();
        this.rCaNDialog = rCaNDialog;
        this.hasToWaitWhenCompleted = hasToWaitWhenCompleted;
        this.secondes = secondes;
        this.caution = caution;
        started = true;
        completed = false;
        secondsAfterStart = 0;
        secondsAfterCompletion = 0;
     }

    @Override
    public void handle(long now) {
        if((now-prevTime)<sleeps){
            return;
        }
        prevTime = now;
        if(secondes != null) {
            secondsAfterStart++;
            secondsAfterCompletion++;
            if (started) {
                secondes.setText("Running RCaN command. Elapsed time : " + secondsAfterStart + " seconds");
            }
            else if(completed && hasToWaitWhenCompleted && secondsAfterCompletion < 1){
                    caution.setText("");
                    secondes.setText(stringResult);
                }
            else {
                rCaNDialog.clearDialog();
                this.stop();
            }
        }
    }

    public void setStringResult(String stringResult) {
        this.stringResult = stringResult;
    }

    public void setCompleted(boolean completed) {
        secondsAfterCompletion = 0;
        started = false;
        this.completed = completed;
    }

 }