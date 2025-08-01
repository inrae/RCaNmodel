/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.objects;

import fr.cm.network.FluxGroup;
import fr.cm.objects.Component;

/**
 *
 * @author christianmullon
 */
public class Flux extends FluxGroup {
    // --------------------------------------------
    public Flux(String i, String o, boolean t) {

        super(i,o,t);
    }
    // --------------------------------------------
    public Flux(Component i, Component o, boolean t) {

        super(i,o,t);
    }
    // ---------------------------------------------------------------------

}
