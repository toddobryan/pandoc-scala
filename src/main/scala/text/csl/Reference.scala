package text.csl

case class Agent (
  givenName: Stream[String],
  droppingPart: String,
  nonDroppingPart: String,
  familyName: String,
  literal: String,
  commaSuffix: Boolean
)

case class RefDate(
  year: String,
  month: String,
  season: String,
  day: String,
  other: String,
  circa: String
)

sealed abstract class RefType
case object NoType extends RefType
case object Article extends RefType
case object ArticleMagazine extends RefType
case object ArticleNewspaper extends RefType
case object ArticleJournal extends RefType
case object Bill extends RefType
case object Book extends RefType
case object Broadcast extends RefType
case object Chapter extends RefType
case object Dataset extends RefType
case object Entry extends RefType
case object EntryDictionary extends RefType
case object EntryEncyclopedia extends RefType
case object Figure extends RefType
case object Graphic extends RefType
case object Interview extends RefType
case object Legislation extends RefType
case object LegalCase extends RefType
case object Manuscript extends RefType
case object Map extends RefType
case object MotionPicture extends RefType
case object MusicalScore extends RefType
case object Pamphlet extends RefType
case object PaperConference extends RefType
case object Patent extends RefType
case object Post extends RefType
case object PostWeblog extends RefType
case object PersonalCommunication extends RefType
case object Report extends RefType
case object Review extends RefType
case object ReviewBook extends RefType
case object Song extends RefType
case object Speech extends RefType
case object Thesis extends RefType
case object Treaty extends RefType
case object Webpage extends RefType

class Reference{
  val refId: String = ""
  val refType: RefType = NoType 
  val author: Stream[Agent] = Stream.Empty
  val editor: Stream[Agent] = Stream.Empty
  val translator: Stream[Agent] = Stream.Empty
  val recipient: Stream[Agent] = Stream.Empty
  val interviewer: Stream[Agent] = Stream.Empty
  val composer: Stream[Agent] =  Stream.Empty
  val director: Stream[Agent] = Stream.Empty
  val illustrator: Stream[Agent] = Stream.Empty
  val originalAuthor: Stream[Agent] = Stream.Empty
  val containerAuthor: Stream[Agent] = Stream.Empty
  val collectionEditor: Stream[Agent] = Stream.Empty
  val editorialDirector: Stream[Agent] = Stream.Empty
  val reviewedAuthor: Stream[Agent] = Stream.Empty
  
  val issued: Stream[RefDate] = Stream.Empty
  val eventDate: Stream[RefDate] = Stream.Empty
  val accessed: Stream[RefDate] = Stream.Empty
  val container: Stream[RefDate] = Stream.Empty
  val originalDate: Stream[RefDate] = Stream.Empty
  val submitted: Stream[RefDate] = Stream.Empty
  
  val title: String = ""
  val titleShort: String = ""
  val reviewedTitle: String = "" 
  val containerTitle: String = ""
  val collectionTitle: String = ""
  val containerTitleShort: String = ""
  val collectionNumber: String = ""
  val originalTitle: String = ""
  val publisher: String = ""
  val originalPublisher: String = ""
  val publisherPlace: String = ""
  val originalPublisherPlace: String = ""
  val authority: String = ""
  val jurisdiction: String = ""
  val archive: String = ""
  val archivePlace: String = ""
  val archiveLocation: String = ""
  val event: String = ""
  val eventPlace: String = ""
  val page: String = ""
  val pageFirst: String = ""
  val numberOfPages: String = ""
  val version: String = ""
  val volume: String = ""
  val numberOfVolumes: String = ""
  val issue: String = ""
  val chapterNumber: String = ""
  val medium: String = ""
  val status: String = ""
  val edition: String = ""
  val section: String = ""
  val source: String = ""
  val genre: String = ""
  val note: String = ""
  val annote: String = ""
  val abstrakt: String = "" // not abstract because keyword
  val keyword: String = ""
  val number: String = ""
  val references: String = ""
  val url: String = ""
  val doi: String = ""
  val isbn: String = ""
  val issn: String = ""
  val pmcid: String = ""
  val pmid: String = ""
  val callNumber: String = ""
  val dimensions: String = ""
  val scale: String = ""
  val categories: Stream[String] = Stream.Empty
    
  val citationNumber: Int = 0
  val firstReferenceNoteNumber: Int = 0
  val citationLabel: String = ""
}
