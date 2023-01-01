package jp.co.future.uroborosql.client.completer;

import java.util.ArrayList;
import java.util.List;
import java.util.ServiceLoader;

import org.jline.reader.LineReader;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import jp.co.future.uroborosql.client.ReaderTestSupport;
import jp.co.future.uroborosql.client.command.ReplCommand;

public class SqlKeywordCompleterTest extends ReaderTestSupport {
	private static List<ReplCommand> commands = new ArrayList<>();

	@BeforeAll
	public static void setUpClass() throws Exception {
		// ReplCommandの読み込み
		for (ReplCommand command : ServiceLoader.load(ReplCommand.class)) {
			commands.add(command);
		}

	}

	@Test
	void testComplete() throws Exception {
		var completer = new SqlKeywordCompleter(commands);
		reader.setCompleter(completer);
		reader.setOpt(LineReader.Option.CASE_INSENSITIVE);

		assertBuffer("qu", new TestBuffer("qu").tab());
		assertBuffer("query sql", new TestBuffer("query sql").tab());
		assertBuffer("generate select ", new TestBuffer("generate se").tab());
		assertBuffer("generate insert ", new TestBuffer("generate in").tab());
		assertBuffer("generate update ", new TestBuffer("generate up").tab());
		assertBuffer("generate delete ", new TestBuffer("generate de").tab());
		assertBuffer("generate delete", new TestBuffer("generate ").tab().tab());
		assertBuffer("generate delete", new TestBuffer("generate ").tab().tab());
		assertBuffer("generate insert", new TestBuffer("generate ").tab().tab().tab());
		assertBuffer("generate select", new TestBuffer("generate ").tab().tab().tab().tab());
		assertBuffer("generate update", new TestBuffer("generate ").tab().tab().tab().tab().tab());
		assertBuffer("generate select ", new TestBuffer("generate select").tab().tab());
		assertBuffer("generate select ", new TestBuffer("generate select ").tab().tab());
		assertBuffer("generate", new TestBuffer("generate").tab());
		assertBuffer("generate ", new TestBuffer("generate ").tab());
	}

}
