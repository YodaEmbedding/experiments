package com.mulhaq.Experimentation1;

import java.util.List;

/**
 * Created by Mateen on 2016-01-01.
 */
public class ListTest {
	static int MAX = 100000;

	static void InitializeList(List<Integer> list) {
		for (int i = 0; i < MAX; i++) {
			list.add(i);
		}
	}

	static long TimeExecution(Runnable func) {
		long startTime = System.currentTimeMillis();
		func.run();
		return System.currentTimeMillis() - startTime;
	}

	static void RunTest(String testName, List<Integer> list) {
		// Initialize test
		switch (0) {
			case 0:
				System.out.println(testName + " initialize: " +
					TimeExecution(new Runnable() {
						@Override
						public void run() {
							InitializeList(list);
						}
					}) + "ms");
				break;
			case 1:
				System.out.println(testName + " initialize: " +
					TimeExecution(() -> InitializeList(list)) + "ms");
				break;
		}

		// Pop FIFO test
		switch (0) {
			case 0:
				final List<Integer> myList = list;
				System.out.println(testName + " remove: " +
					TimeExecution(new Runnable() {
						@Override
						public void run() {
							while (!myList.isEmpty()) {
								myList.remove(0);
							}
						}
					}) + "ms");
				break;

			case 1:
				class Func implements Runnable {
					private List<Integer> list;

					public Func(List<Integer> list) {
						this.list = list;
					}

					@Override
					public void run() {
						while (!list.isEmpty()) {
							list.remove(0);
						}
					}
				}

				System.out.println(testName + " remove: " +
					TimeExecution(new Func(list)) + "ms");
				break;

			case 2:
				System.out.println(testName + " remove: " +
					TimeExecution(new Runnable() {
						List<Integer> myList;

						@Override
						public void run() {
							while (!myList.isEmpty()) {
								myList.remove(0);
							}
						}

						public Runnable init(List<Integer> arg) {
							this.myList = arg;
							return this;
						}
					}.init(list)) + "ms");
				break;
		}
	}
}
