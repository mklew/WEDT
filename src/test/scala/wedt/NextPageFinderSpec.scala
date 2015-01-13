package wedt

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
class NextPageFinderSpec extends FlatSpec with Matchers with SampleData {

  "NextPageFinder" should "find all links in page" in {

  }

  it should "filter links to only those pointing to same url" in {
    // TODO relative and absolute links
  }

  it should "filter links to only those containing key words" in {

  }

  it should "find links with keywords in it" in {

  }

  it should "find links with rel='next'" in {

  }

}
