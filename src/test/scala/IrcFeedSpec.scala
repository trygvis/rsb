import org.specs._
import java.net._

import IrcFeed._

class IrcFeedSpec extends Specification {
    "nodeSeqToAtomDocument" should {
        "be happy" in {
            val document =
            <feed xml:lang="en-US" xmlns:media="http://search.yahoo.com/mrss/" xmlns="http://www.w3.org/2005/Atom">
              <id>tag:github.com,2008:/javaBin/androidito/commits/master</id>
              <link type="text/html" href="http://github.com/javaBin/androidito/commits/master/" rel="alternate"/>
              <link type="application/atom+xml" href="http://github.com/javaBin/androidito/commits/master.atom" rel="self"/>
              <title>Recent Commits to androidito:master</title>
              <updated>2010-04-27T14:12:51-07:00</updated>
              <entry>
                <id>tag:github.com,2008:Grit::Commit/2bee6fa33c99ecc0ebf790fb36b40027547c51e2</id>
                <link type="text/html" href="http://github.com/javaBin/androidito/commit/2bee6fa33c99ecc0ebf790fb36b40027547c51e2" rel="alternate"/>
                <title>Refactored aggregated sessions display (lightning talks) and made the splashcreen logo original size</title>
                <updated>2010-04-27T14:12:51-07:00</updated>
                <content type="html">blah</content>
                <media:thumbnail url="http://www.gravatar.com/avatar/26c1044148676de8efc80af8d011a8ef?s=30&amp;d=http%3A%2F%2Fgithub.com%2Fimages%2Fgravatars%2Fgravatar-30.png"/>
                <author>
                  <name>&#216;yvind L&#248;kling</name>
                </author>
              </entry>
              <entry>
                <id>tag:github.com,2008:Grit::Commit/9d2ad21a6f501df0f49237f4228f9830dfea5d85</id>
                <link type="text/html" href="http://github.com/javaBin/androidito/commit/9d2ad21a6f501df0f49237f4228f9830dfea5d85" rel="alternate"/>
                <title>Lightning talks now aggregated to one session, and sort order for the different views</title>
                <updated>2010-04-25T14:05:40-07:00</updated>
                <content type="html">meh</content>
                <media:thumbnail url="http://www.gravatar.com/avatar/26c1044148676de8efc80af8d011a8ef?s=30&amp;d=http%3A%2F%2Fgithub.com%2Fimages%2Fgravatars%2Fgravatar-30.png"/>
                <author>
                  <name>&#216;yvind L&#248;kling</name>
                </author>
              </entry>
          </feed>
          
          val e1 = AtomEntry("Refactored aggregated sessions display (lightning talks) and made the splashcreen logo original size", "blah")
          val e2 = AtomEntry("Lightning talks now aggregated to one session, and sort order for the different views", "meh")
          nodeSeqToAtomDocument(document) must be equalTo AtomDocument(List(e1, e2))
        }
    }

    "githubRepositoryListToList" should {
        "be happy" in {
            val document = 
            <repositories type="array">
              <repository>
                <url>http://github.com/javaBin/ems</url>
                <name>ems</name>
                <owner>javaBin</owner>
                <open-issues type="integer">0</open-issues>
              </repository>
              <repository>
                <url>http://github.com/javaBin/incogito</url>
                <name>incogito</name>
                <owner>javaBin</owner>
              </repository>
            </repositories>

            githubRepositoryListToList(document) must be equalTo List(("ems", new URL("http://github.com/javaBin/ems/commits/master.atom")), ("incogito", new URL("http://github.com/javaBin/incogito/commits/master.atom")))
        }
    }

/*
    "githubRepositoryToEntries" should {
        "be happy" in {
            val document = 
            <repository>
                <description>Event Management Suite</description>
                <has-issues type="boolean">true</has-issues>
                <watchers type="integer">13</watchers>
                <has-downloads type="boolean">true</has-downloads>
                <homepage>http://smia.java.no</homepage>
                <fork type="boolean">false</fork>
                <forks type="integer">5</forks>
                <url>http://github.com/javaBin/ems</url>
                <private type="boolean">false</private>
                <has-wiki type="boolean">true</has-wiki>
                <name>ems</name>
                <owner>javaBin</owner>
                <open-issues type="integer">0</open-issues>
            </repository>

            githubRepositoryToEntries(document) must be equalTo AtomDocument()
        }
    }
*/
}

