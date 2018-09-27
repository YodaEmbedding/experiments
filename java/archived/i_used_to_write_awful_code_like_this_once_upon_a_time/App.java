package com.mulhaq.Experimentation1;

import com.mulhaq.Experimentation1.SimUDuck;
import com.mulhaq.Experimentation1.SimUDuck.Duck;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 * Created by Mateen on 2015-12-29.
 */
public class App {

	public static void main(String[] args) {
		List<Integer> linkedList = new LinkedList<>();
		List<Integer> arrayList = new ArrayList<>();

		ListTest.RunTest("Linked list", linkedList);
		ListTest.RunTest("Array list", arrayList);

//		MyRegex myRegex = new MyRegex("abc abc Hello!!!!", "abc");
//		MyRegex.Position pos = myRegex.match();
//		System.out.println(pos.toString() + ": " + myRegex.substr(pos));

		Duck[] ducks = {
			new SimUDuck.MallardDuck(),
			new SimUDuck.RedheadDuck(),
			new SimUDuck.RubberDuck()
		};

		for (Duck duck : ducks) {
			System.out.println("");
			duck.printType();
			duck.performFly();
			duck.performQuack();
		}
	}

}
