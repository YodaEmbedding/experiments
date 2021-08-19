import rx
import rx.subject


def test_subject(subject):
    disposable = subject.subscribe(print)
    subject.on_next("Hello, world!")
    subject.on_next("After this line, I am gone.")
    disposable.dispose()
    subject.on_next("I am no more!")


def main():
    subject = rx.subject.Subject()
    test_subject(subject)


main()
