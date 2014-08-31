package Threads;

import java.util.ArrayList;

public class Philo extends Thread {

	private int place = 0;

	public Philo(int place) {
		this.place = place;
	}

	@Override
	public void run() {
		while(true){
		if (place % 5 == 0) {
			forks.get(0).take();
			forks.get(4).take();
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			forks.get(0).drop();
			forks.get(4).drop();
		} else {
			forks.get(place % 5).take();
			forks.get((place % 5)-1).take();
			try {
				Thread.sleep(100);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			forks.get(place % 5).drop();
			forks.get((place % 5)-1).drop();
		}
		System.out.println("Philosoph "+place+" ate.");}
	}

	public static ArrayList<fork> forks = new ArrayList<fork>();

	public static void main(String[] args) {

		for (int i = 0; i < 5; i++) {
			forks.add(new fork());
		}
		for (int i = 0; i < 5; i++) {
			new Philo(i).start();
		}

	}
}
