import org.joda.time._
import org.joda.time.format._

case class AtomDocument(val id: String, val title: String, val updated: DateTime, val entries: List[AtomEntry]) {
    def +(entries: List[AtomEntry]) = new AtomDocument(id, title, updated, this.entries ++ entries)

    def toNodeSeq =
        <feed xml:lang="en-US" xmlns="http://www.w3.org/2005/Atom">
            <id>{id}</id>
            <title>{title}</title>
            <updated>{updated}</updated>
            {entries.map(_.toNodeSeq)}
        </feed>
}

object AtomDocument {
    def apply(id: String, title: String, updated: DateTime): AtomDocument = new AtomDocument(id, title, updated, Nil)

    // this is silly
    val toNodeSeq = {(atomDocument: AtomDocument) => atomDocument.toNodeSeq}

    val formatter = ISODateTimeFormat.dateTimeParser
}

case class AtomAuthor(name: String) {
    def toNodeSeq =
        <author>
            <name>{name}</name>
        </author>
}

case class AtomEntry(title: String, author: AtomAuthor, content: String) {
    def toNodeSeq =
        <entry>
            <title>{title}</title>
            {author.toNodeSeq}
            <content>{content}</content>
        </entry>
}

