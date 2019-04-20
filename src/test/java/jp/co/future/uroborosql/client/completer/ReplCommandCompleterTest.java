package jp.co.future.uroborosql.client.completer;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

import org.jline.reader.LineReader;
import org.junit.BeforeClass;
import org.junit.Test;

import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.command.ReplCommand;

public class ReplCommandCompleterTest extends ReaderTestSupport {
	private static List<ReplCommand> commands = new ArrayList<>();

	@BeforeClass
	public static void setUpClass() throws Exception {
		// ReplCommandの読み込み
		for (ReplCommand command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}

	}

	@Test
	public void testComplete() throws Exception {
		ReplCommandCompleter completer = new ReplCommandCompleter(commands);
		reader.setCompleter(completer);
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("query ", new TestBuffer("qu").tab());
		assertBuffer("update ", new TestBuffer("up").tab());
		assertBuffer("update ", new TestBuffer("UP").tab());
		assertBuffer("update sql", new TestBuffer("update sql").tab());
	}

}
