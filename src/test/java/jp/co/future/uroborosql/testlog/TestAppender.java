package jp.co.future.uroborosql.testlog;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import ch.qos.logback.classic.spi.ILoggingEvent;

public class TestAppender extends AbstractEncodedAppender<ILoggingEvent> {
	private static final ThreadLocal<List<String>> LOGS_LIST = new ThreadLocal<>();

	@FunctionalInterface
	public interface TestRunnable {
		void run() throws Exception;
	}

	public static List<String> getLogbackLogs(TestRunnable runnable) throws Exception {
		List<String> logs = new ArrayList<>();
		try {
			LOGS_LIST.set(logs);
			runnable.run();
		} finally {
			LOGS_LIST.remove();
		}

		return logs;
	}

	@Override
	protected void append(byte[] encodedLog) {
		StringReader reader = new StringReader(toStringLog(encodedLog));
		BufferedReader bufferedReader = new BufferedReader(reader);
		bufferedReader.lines().forEach(this::append);
	}

	private void append(String encodeedLog) {
		List<String> logs = LOGS_LIST.get();
		if (logs != null) {
			logs.add(encodeedLog);
		}
	}

}
