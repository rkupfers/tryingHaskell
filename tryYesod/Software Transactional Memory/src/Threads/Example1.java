package Threads;

import java.util.ArrayList;

public class Example1 {
	static int z = 0;
	static Object lock = new Object();

	public static void main(String[] args) {
		ArrayList<Thread> ths = new ArrayList<Thread>();
		for (int i = 0; i < 4; i++) {
			ths.add(new Thread(new Runnable() {
				@Override
				public void run() {
					for (int j = 0; j < 5000; j++) {
						synchronized (lock) {
							z++;	
						}
					}
				}
			}));
		}
		for (Thread th : ths) {
			th.start();
		}
		for (Thread th : ths) {
			try {
				th.join();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
		System.out.println("Ergebnis: " + z);
	}
}
