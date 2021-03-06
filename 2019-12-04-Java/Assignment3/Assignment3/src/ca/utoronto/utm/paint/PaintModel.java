package ca.utoronto.utm.paint;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

public class PaintModel extends Observable implements Observer {

    public void reset() {
        for (PaintCommand c : this.commands) {
            c.deleteObserver(this);
        }
        this.commands.clear();
        this.setChanged();
        this.notifyObservers();
    }

    public void addCommand(PaintCommand command) {
        this.commands.add(command);
        command.addObserver(this);
        this.setChanged();
        this.notifyObservers();
    }

    private ArrayList<PaintCommand> commands = new ArrayList<>();

    public ArrayList<PaintCommand> getCommands() {
        return commands;
    }

    public void accept(DrawVisitor drawVisitor) {
        for (PaintCommand c : this.commands) {
            c.accept(drawVisitor);
        }
    }

    /**
     * We Observe our model components, the PaintCommands
     */
    @Override
    public void update(Observable o, Object arg) {
        this.setChanged();
        this.notifyObservers();
    }
}
