package Threads;

public class fork {
	private boolean taken = false;

	public synchronized void take() {
		while (taken){try {
			wait();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}};
		taken = true;
	}

	public synchronized void drop() {
		notify();
		taken = false;
	}
}
