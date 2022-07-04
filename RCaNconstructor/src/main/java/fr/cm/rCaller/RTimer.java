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
    Button button;
    long sleeps = 1000000000;
    long prevTime = 0;
    RCaNDialog rCaNDialog;

    public RTimer(RCaNDialog rCaNDialog, boolean hasToWaitWhenCompleted, Label secondes, Label caution, Button button){
        super();
        this.rCaNDialog = rCaNDialog;
        this.hasToWaitWhenCompleted = hasToWaitWhenCompleted;
        this.secondes = secondes;
        this.caution = caution;
        this.button = button;
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
            System.out.println(secondsAfterStart+ " sec");
            if (started) {
                secondes.setText("Running RCaN command. Elapsed time : " + secondsAfterStart + " seconds");
            }
            else if(completed && hasToWaitWhenCompleted && secondsAfterCompletion < 5){
                    caution.setText("");
                    secondes.setText(stringResult);
                }
            else {
                 rCaNDialog.disparait();
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