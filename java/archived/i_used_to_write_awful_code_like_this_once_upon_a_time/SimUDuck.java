package com.mulhaq.Experimentation1;

/**
 * Created by Mateen on 2016-01-01.
 */
public class SimUDuck {

	public static class Duck {
		protected FlyBehavior flyBehavior;
		protected QuackBehavior quackBehavior;

		public void setFlyBehavior(FlyBehavior flyBehavior) {
			this.flyBehavior = flyBehavior;
		}

		public void setQuackBehavior(QuackBehavior quackBehavior) {
			this.quackBehavior = quackBehavior;
		}

		public void performFly() {
			flyBehavior.fly();
		}

		public void performQuack() {
			quackBehavior.quack();
		}

		public void swim() {
			System.out.println("Swim swim!");
		}

		public void printType() {
			System.out.println(toString());
		}
	}

	public interface FlyBehavior {
		void fly();
	}

	public interface QuackBehavior {
		void quack();
	}

	public static class DuckBehavior {

		public static class FlyWithWings implements FlyBehavior {
			public void fly() {
				System.out.println("Fly with wings!");
			}
		}

		public static class FlyNoWay implements FlyBehavior {
			public void fly() {
				System.out.println("Cannot fly. :(");
			}
		}

		public static class QuackNormally implements QuackBehavior {
			public void quack() {
				System.out.println("Quack!");
			}
		}

		public static class QuackSqueak implements QuackBehavior {
			public void quack() {
				System.out.println("Squeak!");
			}
		}

		public static class QuackNoWay implements QuackBehavior {
			public void quack() {
				System.out.println("...");
			}
		}
	}

	public static class MallardDuck extends Duck {
		public MallardDuck() {
			flyBehavior = new DuckBehavior.FlyWithWings();
			quackBehavior = new DuckBehavior.QuackNormally();
		}
	}

	public static class RedheadDuck extends Duck {
		public RedheadDuck() {
			flyBehavior = new DuckBehavior.FlyWithWings();
			quackBehavior = new DuckBehavior.QuackSqueak();
		}
	}

	public static class RubberDuck extends Duck {
		public RubberDuck() {
			flyBehavior = new DuckBehavior.FlyNoWay();
			quackBehavior = new DuckBehavior.QuackNoWay();
		}
	}

}
