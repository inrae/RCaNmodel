package fr.cm.rCaller;

import com.github.rcaller.graphics.SkyTheme;
import com.github.rcaller.rstuff.RCaller;
import com.github.rcaller.rstuff.RCode;

import java.io.File;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Mehmet Hakan Satman
 *
 */
public class SimpleGraphics {

    public SimpleGraphics() {
        try {
            RCaller caller = RCaller.create();
            RCode code = RCode.create();

            code.addRCode("x <- rnorm(30)");
            code.addRCode("y <- rnorm(30)");
            code.addRCode("ols <- lm(y~x)");

            caller.setGraphicsTheme(new SkyTheme());

            File plt = code.startPlot();
            code.addRCode("barplot(x,y)");
            code.addRCode("abline(ols$coefficients[1], ols$coefficients[2])");
            code.addRCode("abline(mean(y), 0)");
            code.addRCode("abline(v = mean(x))");
            code.endPlot();

            caller.setRCode(code);
            caller.runAndReturnResult("ols");
            System.out.println("caracteristiques file");
            System.out.println(plt.getName());
            System.out.println(plt.length());

            code.showPlot(plt);
        } catch (Exception e) {
            Logger.getLogger(SimpleGraphics.class.getName()).log(Level.SEVERE, e.getMessage());
        }
    }

    public static void main(String[] args) {
        new SimpleGraphics();
    }
}