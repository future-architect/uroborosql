package jp.co.future.uroborosql.parser;

import jp.co.future.uroborosql.exception.TokenNotClosedRuntimeException;

/**
 * SQL分割処理実装クラス
 *
 * @author H.Sugimoto
 */
public class SqlTokenizerImpl implements SqlTokenizer {
	/** SQL文 */
	private final String sql;

	/** 現在地 */
	private int position = 0;

	/** トークン */
	private String token;

	/** トークン種別 */
	private TokenType tokenType = TokenType.SQL;

	/** 次のトークン種別 */
	private TokenType nextTokenType = TokenType.SQL;

	/** バインド変数採番番号 */
	private int bindVariableNumber = 0;

	/**
	 * コンストラクタ
	 *
	 * @param sql 解析対象SQL
	 */
	public SqlTokenizerImpl(final String sql) {
		this.sql = sql;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getPosition()
	 */
	@Override
	public int getPosition() {
		return position;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getToken()
	 */
	@Override
	public String getToken() {
		return token;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getBefore()
	 */
	@Override
	public String getBefore() {
		return sql.substring(0, position);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getAfter()
	 */
	@Override
	public String getAfter() {
		return sql.substring(position);
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getTokenType()
	 */
	@Override
	public TokenType getTokenType() {
		return tokenType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#getNextTokenType()
	 */
	@Override
	public TokenType getNextTokenType() {
		return nextTokenType;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#next()
	 */
	@Override
	public TokenType next() {
		if (position >= sql.length()) {
			token = null;
			tokenType = TokenType.EOF;
			nextTokenType = TokenType.EOF;
			return tokenType;
		}
		switch (nextTokenType) {
		case SQL:
			parseSql();
			break;
		case COMMENT:
			parseComment();
			break;
		case ELSE:
			parseElse();
			break;
		case BIND_VARIABLE:
			parseBindVariable();
			break;
		default:
			parseEof();
			break;
		}
		return tokenType;
	}

	/**
	 * SQL文解析
	 */
	protected void parseSql() {
		int commentStartPos = sql.indexOf("/*", position);
		int lineCommentStartPos = sql.indexOf("--", position);
		int bindVariableStartPos = sql.indexOf("?", position);
		int elseCommentStartPos = -1;
		if (lineCommentStartPos >= 0) {
			int skipPos = skipWhitespace(lineCommentStartPos + 2);
			if (skipPos + 4 < sql.length() && "ELSE".equals(sql.substring(skipPos, skipPos + 4))) {
				elseCommentStartPos = lineCommentStartPos;
			}
		}
		int nextStartPos = getNextStartPos(commentStartPos, elseCommentStartPos, bindVariableStartPos);
		if (nextStartPos < 0) {
			token = sql.substring(position);
			nextTokenType = TokenType.EOF;
			position = sql.length();
			tokenType = TokenType.SQL;
		} else {
			token = sql.substring(position, nextStartPos);
			tokenType = TokenType.SQL;
			boolean needNext = nextStartPos == position;
			if (nextStartPos == commentStartPos) {
				nextTokenType = TokenType.COMMENT;
				position = commentStartPos + 2;
			} else if (nextStartPos == elseCommentStartPos) {
				nextTokenType = TokenType.ELSE;
				position = elseCommentStartPos + 2;
			} else if (nextStartPos == bindVariableStartPos) {
				nextTokenType = TokenType.BIND_VARIABLE;
				position = bindVariableStartPos;
			}
			if (needNext) {
				next();
			}
		}
	}

	/**
	 * 次の解析開始位置を取得
	 *
	 * @param commentStartPos コメント開始位置
	 * @param elseCommentStartPos ELSEコメント開始位置
	 * @param bindVariableStartPos バインド変数開始位置
	 * @return 次の解析位置
	 */
	protected int getNextStartPos(final int commentStartPos, final int elseCommentStartPos,
			final int bindVariableStartPos) {

		int nextStartPos = -1;
		if (commentStartPos >= 0) {
			nextStartPos = commentStartPos;
		}
		if (elseCommentStartPos >= 0 && (nextStartPos < 0 || elseCommentStartPos < nextStartPos)) {
			nextStartPos = elseCommentStartPos;
		}
		if (bindVariableStartPos >= 0 && (nextStartPos < 0 || bindVariableStartPos < nextStartPos)) {
			nextStartPos = bindVariableStartPos;
		}
		return nextStartPos;
	}

	/**
	 * 次のバインド変数名取得
	 *
	 * @return バインド変数名
	 */
	protected String nextBindVariableName() {
		return "$" + ++bindVariableNumber;
	}

	/**
	 * コメント部解析
	 */
	protected void parseComment() {
		int commentEndPos = sql.indexOf("*/", position);
		if (commentEndPos < 0) {
			throw new TokenNotClosedRuntimeException(sql.substring(position));
		}
		token = sql.substring(position, commentEndPos);
		nextTokenType = TokenType.SQL;
		position = commentEndPos + 2;
		tokenType = TokenType.COMMENT;
	}

	/**
	 * バインド変数解析
	 */
	protected void parseBindVariable() {
		token = nextBindVariableName();
		nextTokenType = TokenType.SQL;
		position += 1;
		tokenType = TokenType.BIND_VARIABLE;
	}

	/**
	 * ELSE文解析
	 */
	protected void parseElse() {
		int crPos = sql.indexOf("\n", position);
		String elseToken;
		if (crPos >= 0) {
			elseToken = sql.substring(position, crPos + 1);
		} else {
			elseToken = sql.substring(position, sql.length());
		}
		while (elseToken.endsWith("\n") || elseToken.endsWith("\r")) {
			elseToken = elseToken.substring(0, elseToken.length() - 1);
		}
		token = elseToken;
		nextTokenType = TokenType.SQL;
		position = position + elseToken.length();
		tokenType = TokenType.ELSE;
	}

	/**
	 * EOF
	 */
	protected void parseEof() {
		token = null;
		tokenType = TokenType.EOF;
		nextTokenType = TokenType.EOF;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#skipToken()
	 */
	@Override
	public String skipToken() {
		int length = sql.length();
		int endIndex = length;
		char quote = position < length ? sql.charAt(position) : '\0';
		boolean quoting = quote == '\'' || quote == '(';
		if (quote == '(') {
			quote = ')';
		}
		for (int i = quoting ? position + 1 : position; i < length; ++i) {
			char c = sql.charAt(i);
			if ((Character.isWhitespace(c) || c == ',' || c == ')' || c == '(') && !quoting) {
				endIndex = i;
				break;
			} else if (c == '/' && i + 1 < length && sql.charAt(i + 1) == '*') {
				endIndex = i;
				break;
			} else if (c == '-' && i + 1 < length && sql.charAt(i + 1) == '-') {
				endIndex = i;
				break;
			} else if (quoting && quote == '\'' && c == '\'' && (i + 1 >= length || sql.charAt(i + 1) != '\'')) {
				endIndex = i + 1;
				break;
			} else if (quoting && c == quote) {
				endIndex = i + 1;
				break;
			}
		}
		token = sql.substring(position, endIndex);
		tokenType = TokenType.SQL;
		nextTokenType = TokenType.SQL;
		position = endIndex;
		return token;
	}

	/**
	 * {@inheritDoc}
	 *
	 * @see jp.co.future.uroborosql.parser.SqlTokenizer#skipWhitespace()
	 */
	@Override
	public String skipWhitespace() {
		int index = skipWhitespace(position);
		token = sql.substring(position, index);
		position = index;
		return token;
	}

	/**
	 * ホワイトスペーススキップ
	 *
	 * @param position ポジション
	 * @return 空白をスキップした位置
	 */
	private int skipWhitespace(final int position) {
		int index = sql.length();
		for (int i = position; i < index; ++i) {
			char c = sql.charAt(i);
			if (!Character.isWhitespace(c)) {
				return i;
			}
		}
		return index;
	}

}
