---
layout: page
title: "Publications"
category: doc
date: 2016-07-27 15:32:00
order: 1
---

<body>

<table>

<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="procter15">1</a>]
</td>
<td class="bibtexitem">
Adam Procter, William&nbsp;L. Harrison, Ian Graves, Michela Becchi, and Gerard
  Allwein.
 Semantics driven hardware design, implementation, and verification
  with rewire.
 In <em>Proceedings of the 16th ACM SIGPLAN/SIGBED Conference on
  Languages, Compilers and Tools for Embedded Systems 2015 CD-ROM</em>, LCTES'15,
  pages 13:1--13:10, 2015.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/lctes15.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
There is no such thing as high assurance without high assurance hardware. High assurance hardware is essential, because any and all high assurance systems ultimately depend on hardware that conforms to, and does not undermine, critical system properties and invariants. And yet, high assurance hardware development is stymied by the conceptual gap between formal methods and hardware description languages used by engineers. This paper presents ReWire, a functional programming language providing a suitable foundation for formal verification of hardware designs, and a compiler for that language that translates high-level, semantics-driven designs directly into working hardware. ReWire's design and implementation are presented, along with a case study in the design of a secure multicore processor, demonstrating both ReWire's expressiveness as a programming language and its power as a framework for formal, high-level reasoning about hardware systems.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="graves15">2</a>]
</td>
<td class="bibtexitem">
Ian Graves, Adam Procter, William&nbsp;L. Harrison, Michela Becchi, and Gerard
  Allwein.
 Hardware synthesis from functional embedded domain-specific
  languages: A case study in regular expression compilation.
 In Kentaro Sano, Dimitrios Soudris, Michael Hübner, and Pedro&nbsp;C.
  Diniz, editors, <em>Applied Reconfigurable Computing</em>, volume 9040 of <em>
  Lecture Notes in Computer Science</em>, pages 41--52. 2015.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/arc15.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Although FPGAs have the potential to bring software-like flexibility and agility to the hardware world, designing for FPGAs remains a difficult task divorced from standard software engineering norms. A better programming flow would go far towards realizing the potential of widely deployed, programmable hardware. We propose a general methodology based on domain specific languages embedded in the functional language Haskell to bridge the gap between high level abstractions that support programmer productivity and the need for high performance in FPGA circuit implementations. We illustrate this methodology with a framework for regular expression to hardware compilers, written in Haskell, that supports high programmer productivity while producing circuits whose performance matches and, indeed, exceeds that of a state of the art, hand-optimized VHDL-based tool. For example, after applying a  novel optimization pass, throughput increased an average of 28.3 percent over the state of the art tool for one set of benchmarks.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="icfpt13">3</a>]
</td>
<td class="bibtexitem">
Adam Procter, William&nbsp;L. Harrison, Ian Graves, Michela Becchi, and Gerard
  Allwein.
 Semantics-directed machine architecture in rewire.
 In <em>Proceedings of the 2013 International Conference on Field
  Programmable Technology</em>, pages 446--449, Dec 2013.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/icfpt13.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
The functional programming community has developed a number of powerful abstractions for dealing with diverse programming models in a modular way. Beginning with a core of pure, side effect free computation, modular monadic semantics (MMS) allows designers to construct domain-specific languages by adding layers of semantic features, such as mutable state and I/O, in an a' la carte fashion. In the realm of interpreter and compiler construction, the benefits of this approach are manifold and well explored. This paper advocates bringing the tools of MMS to bear on hardware design and verification. In particular, we shall discuss a prototype compiler called ReWire which translates high-level MMS hardware specifications into working circuits on FPGAs. This enables designers to tackle the complexity of hardware design in a modular way, without compromising efficiency.
</font></blockquote>
<p><blockquote><font size="-1">
Keywords: electronic engineering computing;field programmable gate arrays;formal verification;functional programming;program compilers;FPGA;MMS;ReWire;compiler construction;diverse programming models;domain-specific languages;functional programming;hardware design;modular monadic semantics;semantics-directed machine architecture;verification;Field programmable gate arrays;Hardware;Microcontrollers;Ports (Computers);Random access memory;Registers;Semantics
</font></blockquote>

</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="qasa13">4</a>]
</td>
<td class="bibtexitem">
Robert Harrison and William&nbsp;L. Harrison.
 Quantitative analysis of error injection covert channels.
 In <em>Proceedings of the International Workshop on Quantitative
  Aspects in Security Assurance (QASA13)</em>, 2013.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/interchannel.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Covert channels are mechanisms for communication where a legitimate channel carries a hidden message, and where the hidden message is conveyed using legal operations of the legitimate channel. Quantitative modeling of the response of the covert channel to noise results in an enhanced understanding of the channel and delimits the range of conditions under which a covert channel can operate effectively. The analysis of two covert channels using the confusion matrix and noise models shows that the techniques presented in this paper are widely applicable to covert information flows in noisy channels.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="LLP13">5</a>]
</td>
<td class="bibtexitem">
Gerard Allwein, William Harrison, and David Andrews.
 Simulation logic.
 <em>Logic and Logical Philosophy</em>, 0(0), 2013.
[&nbsp;<a href="http://wydawnictwoumk.pl/czasopisma/index.php/LLP/article/view/LLP.2013.027">http</a>&nbsp;]
<blockquote><font size="-1">
Simulation relations have been discovered in many areas: Computer Science, philosophical and modal logic, and set theory. However, the simulation condition is strictly a first-order logic statement. We extend modal logic with modalities and axioms, the latter's modeling conditions are the simulation conditions. The modalities are normal, i.e., commute with either conjunctions or disjunctions and preserve either Truth or Falsity (respectively). The simulations are considered arrows in a category where the objects are descriptive, general frames. One can augment the simulation modalities by axioms for requiring the underlying modeling simulations to be bisimulations or to be p-morphisms. The modal systems presented are multi-sorted and both sound and complete with respect to their algebraic and Kripke semantics.
</font></blockquote>
<p><blockquote><font size="-1">
Keywords: modal logic; simulations; Hilbert systems; Kripke; modal algebras
</font></blockquote>

</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="ICFEM12">6</a>]
</td>
<td class="bibtexitem">
Adam&nbsp;Procter William L.&nbsp;Harrison and Gerard Allwein.
 The confinement problem in the presence of faults.
 In <em>Proceedings of the 2012 International Conference on Formal
  Engineering Methods</em>, 2012.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/icfem12.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
In this paper, we establish a semantic foundation for the safe
execution of untrusted code. Our approach extends Moggi's computational &lambda;-calculus in two dimensions with operations for asynchronous concurrency, shared state and software faults and with an effect type system <em>a</em> la Wadler providing fine-grained control of effects. An equational system for fault isolation is exhibited and its soundness demonstrated with a semantics based on monad transformers. Our formalization of the equational system in the Coq theorem prover is discussed. We argue that the approach may be generalized to capture other safety properties, including information flow security.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="SSV12">7</a>]
</td>
<td class="bibtexitem">
William L.&nbsp;Harrison Chris&nbsp;Hathhorn, Michela&nbsp;Becchi and Adam Procter.
 Formal semantics of heterogeneous cuda-c: A modular approach with
  applications.
 In <em>Proceedings of the 2012 Systems Software Verification
  Conference</em>, 2012.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/ssv12.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
We extend an off-the-shelf, executable formal semantics of C (Ellison and Rosiu's K Framework semantics) with the core features of CUDA-C. The hybrid CPU/GPU computation model of CUDA-C presents challenges not just for programmers, but also for practitioners of formal methods. Our formal semantics helps expose and clarify these issues. We demonstrate the usefulness of our semantics by generating a tool from it
capable of detecting some race conditions and deadlocks in CUDA-C programs. We discuss limitations of our model and argue that its extensibility can easily
enable a wider range of verification tasks.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="NCL12">8</a>]
</td>
<td class="bibtexitem">
William L.&nbsp;Harrison Gerard&nbsp;Allwein and David Andrews.
 Simulation logic.
 In <em>Proceedings of the 2012 Conference on Non-Classical Logics</em>,
  2012.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/ncl12.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Simulation relations have been discovered in many areas: Computer Science, philosophical and modal logic, and set theory. However, the simulation condition is strictly a first-order logic statement. We extend modal logic with modalities and axioms, the latter's modeling conditions are the simulation conditions. The modalities are normal, i.e., commute with either conjunctions or disjunctions and preserve either Truth or Falsity (respectively). The simulations are considered arrows in a category where the objects are descriptive, general frames. One can augment the simulation modalities by axioms for requiring the underlying modeling simulations to be bisimulations or to be p-morphisms. The modal systems presented are multi-sorted and both sound and complete with respect to their algebraic and Kripke semantics.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="TFP12">9</a>]
</td>
<td class="bibtexitem">
William L.&nbsp;Harrison Adam&nbsp;Procter and Aaron Stump.
 The design of a practical proof checker for a lazy functional
  language.
 In <em>Proceedings of the 2012 Trends in Functional Programming
  Conference</em>, 2012.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/tfp12.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Pure, lazy functional languages like Haskell provide a sound basis for formal reasoning about programs in an equational style. In
practice, however, equational reasoning is underutilized. We suggest that part
of the reason for this is the lack of accessible tools for developing
machine-checked equational reasoning proofs. This paper outlines the design of
MProver, a system which fills just that niche. MProver features first-class
support for reasoning about potentially undefined computations (particularly
important in a lazy setting), and an extended notion of Haskell-like type
classes, enabling a highly modular style of program verification that closely
follows familiar functional programming idioms.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="ERSA2011a">10</a>]
</td>
<td class="bibtexitem">
Gerard Allwein and William&nbsp;L. Harrison.
 A channel theoretic account of separation security.
 In <em>Proceedings of the 2011 International Conference on
  Engineering Reconfigurable Systems and Algorithms (ERSA11)</em>, 2011.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/ersa2011a.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
It has long been held that information flow security models should be organized with respect to a theory of information, but typically they are not. The appeal of a information-theoretic foundation for information flow security seems natural, compelling and, indeed, almost tautological. This article illustrates how channel theory---a theory of information based in logic---can provide a basis for noninterference style security models. The evidence presented here suggests that channel theory is a useful organizing principle for information flow security.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="ERSA2011b">11</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison, Benjamin Schulz, Adam Procter, Andrew Lukefahr, and Gerard
  Allwein.
 Towards semantics-directed system design and synthesis.
 In <em>Proceedings of the 2011 International Conference on
  Engineering Reconfigurable Systems and Algorithms (ERSA11)</em>, 2011.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/ersa2011b.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
High assurance systems have been defined as systems &ldquo;you would bet your life on.&rdquo; This article discusses the application of a form of functional programming---what we call &ldquo;monadic programming&rdquo;---to the generation of high assurance and secure systems. Monadic programming languages leverage algebraic structures from denotational semantics and functional programming---monads---as a flexible, modular organizing principle for secure system design and implementation. Monadic programming languages are domain-specific functional languages that are both sufficiently expressive to express essential system behaviors and semantically straightforward to support formal verification.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="LLP10">12</a>]
</td>
<td class="bibtexitem">
Gerard Allwein, Yingrui Yang, and William&nbsp;L. Harrison.
 Qualitative decision theory via channel theory.
 <em>Logic and Logical Philosophy</em>, pages 81--110, 2011.
 Extended version of [<a href="#AllweinYangHarrison:2010">13</a>].
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/llp2011.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
We recast parts of decision theory in terms of channel theory
concentrating on qualitative issues. Channel theory allows one to move
between model theoretic and language theoretic notions as is necessary for
an adequate covering. Doing so clari?es decision theory and presents the
opportunity to investigate alternative formulations. As an example, we take
some of Savage's notions of decision theory and recast them within channel
theory. In place of probabilities, we use a particular logic of preference. We
introduce a logic for describing actions separate from the logic of preference
over actions. The structures introduced by channel theory that represent
the decision problems can be seen to be an abstract framework. This framework is very accommodating to changing the nature of the decision problems
to handle different aspects or theories about decision making.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="AllweinYangHarrison:2010">13</a>]
</td>
<td class="bibtexitem">
Gerard Allwein, Yingrui Yang, and William&nbsp;S. Harrison.
 Decision theory via channel theory.
 In <em>Proceedings of the Logic in Cognitive Science Conference,
  2010, Logic and Logical Philosophy Journal</em>. The Nicolaus Copernicus
  University Press, 2010.

</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="AiML10">14</a>]
</td>
<td class="bibtexitem">
Gerard Allwein and William&nbsp;L. Harrison.
 Partially-ordered modalities.
 In <em>Advances in Modal Logic</em>, pages 1--21, 2010.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/aiml10.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">

Modal logic is extended by partially ordering the modalities. The modalities are normal, i.e., commute
with either conjunctions or disjunctions and preserve either Truth or Falsity (respectively). The partial
order does not conflict with type of modality (K, S4, etc.) although this paper will concentrate on S4
since partially ordered S4 systems appear to be numerous. The partially-ordered normal modal systems
considered are both sound and complete. Hilbert and Gentzen systems are given. A cut-elimination theorem
holds (for partially ordered S4), and the Hilbert and Gentzen systems present the same logic. The partial
order induces a 2-category structure on a coalgebraic formulation of descriptive frames. Channel theory is
used to `move' modal logics when the source and target languages may be dierent. A particular partially
ordered modal system is shown to be applicable to security properties
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="dslwc09">15</a>]
</td>
<td class="bibtexitem">
W.&nbsp;Harrison, A.&nbsp;Procter, J.&nbsp;Agron, G.&nbsp;Kimmel, and G.&nbsp;Allwein.
 Model-driven engineering from modular monadic semantics:
  Implementation techniques targeting hardware and software.
 In <em>DSL '09: Proc. of the IFIP TC 2 Working Conference on
  Domain-Specific Languages</em>, pages 20--44, 2009.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/dsl09.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Recent research has shown how the formal modeling of concurrent systems can benefit from monadic structuring. With this approach, a formal system model is really a program in a domain specific language defined by a monad for shared-state concurrency. Can these models be compiled into efficient implementations?  This paper addresses this question and presents an overview of techniques for compiling monadic concurrency models directly into reasonably efficient software and hardware implementations. The implementation techniques described in this article form the basis of a semantics-directed approach to model-driven engineering.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HarrisonHook09">16</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison and James Hook.
 Achieving information flow security through monadic control of
  effects.
 <em>J. Comput. Secur.</em>, 17:599--653, October 2009.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/jcs09.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This paper advocates a novel approach to the construction of secure
software: controlling information flow and maintaining integrity via
monadic encapsulation of effects. This approach is <em>constructive</em>,
relying on properties of monads and monad transformers to build,
verify, and extend secure software systems. We illustrate this
approach by construction of abstract operating systems called <em>
  separation kernels</em>. Starting from a mathematical model of
shared-state concurrency based on monads of resumptions and state, we
outline the development by stepwise refinements of separation kernels
supporting Unix-like system calls, interdomain communication,  and a
formally verified security policy (domain separation). Because monads
may be easily and safely represented within any pure, higher-order,
typed functional language, the resulting system models may be directly
realized within a language such as Haskell.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HarrisonAllwein08">17</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison, Gerard Allwein, Andy Gill, and Adam Procter.
 Asynchronous exceptions as an effect.
 In <em>Proceedings of the Mathematics of Program Construction
  (MPC08)</em>, pages 153--176, 2008.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/mpc08.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Asynchronous interrupts abound in computing systems, yet they remain a
thorny concept for both programming and verification practice.
The ubiquity of interrupts underscores the importance of developing
programming models to aid the development and verification of
interrupt-driven programs. 
The research reported here recognizes asynchronous interrupts as a
computational effect and encapsulates them as a building block in
modular monadic semantics.
The resulting modular semantic model can serve as both a guide for
functional programming with interrupts and as a formal basis for reasoning about
interrupt-driven computation as well.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="Haskell08">18</a>]
</td>
<td class="bibtexitem">
Pericles&nbsp;S. Kariotis, Adam&nbsp;M. Procter, and William&nbsp;L. Harrison.
 Making monads first-class with template haskell.
 In <em>Proceedings of the first ACM SIGPLAN Symposium on Haskell</em>,
  Haskell '08, pages 99--110, New York, NY, USA, 2008. ACM.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/haskell08.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Monads as an organizing principle for programming and semantics are notoriously difficult to grasp, yet they are a central and powerful abstraction in Haskell. This paper introduces a domain-specific language, MonadLab, that simplifies the construction of monads, and describes its implementation in Template Haskell. MonadLab makes monad construction truly first class, meaning that arcane theoretical issues with respect to monad transformers are completely hidden from the programmer. The motivation behind the design of MonadLab is to make monadic programming in Haskell simpler while providing a tool for non-Haskell experts that will assist them in understanding this powerful abstraction.
</font></blockquote>
<p><blockquote><font size="-1">
Keywords: domain-specific languages, monads, staged programming
</font></blockquote>

</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="CheapThreads">19</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison.
 Cheap (but functional) threads.
 <em>Higher Order and Symbolic Computation (accepted for
  publication)</em>.
 45 pages. Available by request.
<blockquote><font size="-1">
This article demonstrates how a powerful and expressive abstraction 
from concurrency theory plays a dual role as a programming tool 
for concurrent applications and as a foundation for their verification.
This abstraction---monads of resumptions expressed using monad 
transformers---is <em>cheap</em>: it is easy to understand, easy to 
implement, and easy to reason about.
We illustrate the expressiveness of the resumption monad with the 
construction of an exemplary multitasking operating system kernel 
with process forking, preemption, message passing, and 
synchronization constructs in the pure functional programming 
language Haskell.
Because resumption computations are stream-like structures, reasoning 
about this kernel may be posed as reasoning about streams, a problem 
which is well-understood. And, as an example, this article 
illustrates how a liveness property---fairness---is proven and 
especially how the resumption-monadic structure promotes such 
verifications.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="APLAS06">20</a>]
</td>
<td class="bibtexitem">
William Harrison.
 Proof abstraction for imperative languages.
 In <em>Proceedings of the 4th Asian Symposium on Programming
  Languages and Systems (APLAS06)</em>, pages 97--113, 2006.
<blockquote><font size="-1">
Modularity in programming language semantics derives from abstracting over
the structure of underlying denotations, yielding semantic descriptions that are more abstract and reusable. One such semantic framework is Liang's modular
monadic semantics in which the underlying semantic structure is encapsulated with a monad. Such abstraction can be at odds with
program verification, however, because program specifications require access to the (deliberately) hidden semantic
representation. The techniques for reasoning about modular monadic definitions of imperative programs introduced here overcome this barrier.
And, just like program definitions in modular monadic semantics, our program specifications and proofs are representation-independent
and hold for whole classes of monads, thereby yielding proofs of great generality.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="harrison06">21</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison.
 The essence of multitasking.
 In <em>11th International Conference on Algebraic Methodology and
  Software Technology (AMAST 2006)</em>, pages 158--172, July 2006.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/amast06.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This article demonstrates how a powerful and expressive abstraction from concurrency theory---monads of resumptions---plays a dual r&ocirc;le as a programming tool
for concurrent applications.  
The article demonstrates how a
wide variety of typical OS behaviors may be specified in terms of
resumption monads known heretofore exclusively in the literature of 
programming language semantics. 
We illustrate the expressiveness of the resumption monad with the
construction of an exemplary 
multitasking kernel in the pure functional language
Haskell.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="ijdmb06">22</a>]
</td>
<td class="bibtexitem">
X.&nbsp;Z. Fu, H.&nbsp;Wang, W.&nbsp;Harrison, and R.&nbsp;Harrison.
 RNA pseudoknot prediction using term rewriting.
 <em>International Journal of Data Mining and Bioinformatics</em>, 2005.
<blockquote><font size="-1">
RNA plays a critical role in mediating every step of cellular information transfer from genes to functional proteins. Pseudoknots are functionally important and widely occurring structural motifs found in all types of RNA. Therefore predicting their structures is an important problem. In this paper, we present a new RNA pseudoknot structure prediction method based on term rewriting. The method is implemented using the Mfold RNA/DNA folding package and the term rewriting language Maude. In our method, RNA structures are treated as terms and rules are discovered for predicting pseudoknots. Our method was tested on 211 pseudoknots in PseudoBase and achieves an average accuracy of 74.085 percent compared to the experimentally determined structure. In fact, most pseudoknots discovered by our method achieve an accuracy of above 90 percent. These results indicate that term rewriting has a broad potential in RNA applications ranging from prediction of pseudoknots to discovery of higher level RNA structures involving complex RNA tertiary interactions.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HK05">23</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison and Richard&nbsp;B. Kieburtz.
 The logic of demand in Haskell.
 <em>Journal of Functional Programming</em>, 15(6):837--891, 2005.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/jfp05.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Haskell is a functional programming language whose
                  evaluation is lazy by default. However, Haskell also
                  provides pattern matching facilities which add a
                  modicum of eagerness to its otherwise lazy default
                  evaluation. This mixed or <em>non-strict</em>
                  semantics can be quite difficult to reason
                  with. This paper introduces a programming logic,
                  P-Logic, which neatly formalizes the mixed
                  evaluation in Haskell pattern-matching as a logic,
                  thereby simplifying the task of specifying and
                  verifying Haskell programs. In P-Logic, aspects of
                  demand are reflected or represented within both the
                  predicate language and its model theory, allowing
                  for expressive and comprehensible program
                  verification.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="APLAS05">24</a>]
</td>
<td class="bibtexitem">
William Harrison.
 A simple semantics for polymorphic recursion.
 In <em>Proceedings of the 3rd Asian Symposium on Programming
  Languages and Systems (APLAS05)</em>, pages 37--51, Tsukuba, Japan, November
  2005.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/aplas05.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Polymorphic recursion is a useful extension of Hindley-
Milner typing and has been incorporated in the functional programming
language Haskell. It allows the expression of efficient algorithms
that take advantage of non-uniform data structures and provides key
support for generic programming. However, polymorphic recursion is,
perhaps, not as broadly understood as it could be and this, in part,
motivates the denotational semantics presented here. The semantics reported
here also contributes an essential building block to any semantics
of Haskell: a model for first-order polymorphic recursion. Furthermore,
Haskell-style type classes may be described within this semantic framework
in a straightforward and intuitively appealing manner.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="BIBE05">25</a>]
</td>
<td class="bibtexitem">
X.&nbsp;Z. Fu, H.&nbsp;Wang, W.&nbsp;Harrison, and R.&nbsp;Harrison.
 RNA pseudoknot prediction using term rewriting.
 In <em>Proceedings of IEEE Fifth Symposium on Bioinformatics and
  Bioengineering (BIBE05)</em>, pages 169--176, Minneapolis, MN, October 2005.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/bibe05.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
RNA plays a critical role in mediating every step of cellular information transfer from genes to functional proteins. Pseudoknots are widely occurring structural  motifs found in all types of RNA and are also functionally important. Therefore predicting their structures is an important problem. In this paper, we present a new RNA pseudoknot prediction model based on term rewriting rather than on dynamic programming, comparative sequence analysis, or context-free grammars. The model we describe is implemented using the Mfold RNA/DNA folding package and the term rewriting language Maude. Our model was tested on 211 pseudoknots in PseudoBase and achieves an average accuracy of 74.085% compared to the experimentally determined structure. In fact, most pseudoknots discovered by our method achieve an accuracy of above 90%. These results indicate that term rewriting has a broad potential in RNA applications from prediction of pseudoknots to higher level RNA structures involving complex RNA tertiary interactions.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="CSFW05">26</a>]
</td>
<td class="bibtexitem">
William Harrison and James Hook.
 Achieving information flow security through precise control of
  effects.
 In <em>18th IEEE Computer Security Foundations Workshop (CSFW05)</em>,
  pages 16--30, Aix-en-Provence, France, June 2005.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/csfw05.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This paper advocates controlling information flow and maintaining integrity via monadic encapsulation of effects. This approach is constructive, relying on properties of monads and monad transformers to build, verify, and extend secure software systems. We illustrate this approach by construction of abstract operating systems called separation kernels.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HarrisonHarrison04">27</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison and Robert&nbsp;W. Harrison.
 Domain specific languages for cellular interactions.
 In <em>Proceedings of the 26th Annual IEEE International Conference
  on Engineering in Medicine and Biology (EMBC04)</em>, September 2004.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/embc04.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Bioinformatics is the application of Computer Science techniques to problems in Biology, and this paper explores one such application with great potential: the modeling of life cycles of autonomous, intercommunicating cellular systems using domain-specific programming languages (DSLs). We illustrate this approach for the simple photo-synthetic bacterium <em>R. Sphaeroides</em> with a DSL called CellSys embedded in the programming language Haskell.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="DomSep03">28</a>]
</td>
<td class="bibtexitem">
William Harrison, Mark Tullsen, and James Hook.
 Domain separation by construction.
 In <em>LICS03 Satellite Workshop on Foundations of Computer Security
  (FCS03)</em>, June 2003.
 21 pages.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/fcs03.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This paper advocates a novel approach to language-based security: by structuring software with monads (a form of abstract data type for effects), we are able to maintain separation of effects by construction. The thesis of this work is that well-understood properties of monads and monad transformers aid in the construction and verification of secure software. We introduce a formulation of non-interference based on monads rather than the typical trace-based formulation. Using this formulation, we prove a non-interference style property for a simple instance of our system model. Because monads may be easily and safely represented within any pure, higher-order, typed functional language, the resulting system models may be directly realized within such a language.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HK02">29</a>]
</td>
<td class="bibtexitem">
William Harrison and Richard Kieburtz.
 Pattern-driven reduction in haskell.
 In <em>2nd International Workshop on Reduction Strategies in
  Rewriting and Programming (WRS02)</em>, Copenhagen, Denmark, 2002.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/wrs02.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Haskell is a functional programming language with nominally non-strict
semantics, implying that evaluation of a Haskell expression proceeds by
demand-driven reduction.  However, Haskell also provides pattern 
matching
on arguments of functions, in <b>let</b> expressions and in the match
clauses of <b>case</b> expressions.  Pattern-matching requires
data-driven reduction to the extent necessary to
evaluate a pattern match or to bind variables introduced in a pattern.
In this paper we
provide both an abstract semantics and a logical characterization of
pattern-matching in Haskell and the reduction order that it entails.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="FCDH2002">30</a>]
</td>
<td class="bibtexitem">
William Harrison, Timothy Sheard, and James Hook.
 Fine control of demand in Haskell.
 In <em>6th International Conference on the Mathematics of Program
  Construction (MPC02), Dagstuhl, Germany</em>, volume 2386 of <em>Lecture Notes
  in Computer Science</em>, pages 68--93. Springer-Verlag, 2002.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/mpc02.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Functional languages have the lambda calculus at their core, but then depart from this firm foundation by including features that alter their default evaluation order. The resulting mixed evaluation---partly lazy and partly strict---complicates the formal semantics of these languages. The functional language Haskell is such a language, with features such as pattern-matching, case expressions with guards, etc., introducing a modicum of strictness into the otherwise lazy language. But just how does Haskell differ from the lazy lambda calculus? We answer this question by introducing a calculational semantics for Haskell that exposes the interaction of its strict features with its default laziness. In the semantics, features perturbing Haskell's standard lazy evaluation order are specified computationally (i.e., monadically) rather than as pure values (i.e., functions, scalars, etc.).
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HarrisonSheard2001">31</a>]
</td>
<td class="bibtexitem">
Bill Harrison and Tim Sheard.
 Dynamically adaptable software with metacomputations in a staged
  language.
 In <em>Proceedings of the Second International Workshop on
  Semantics, Applications, and Implementation of Program Generation (SAIG)</em>,
  volume 2196 of <em>Lecture Notes in Computer Science</em>, pages 163--182,
  Florence, Italy, 2001. Springer-Verlag.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/saig01.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
Profile-driven compiler optimizations take advantage of information gathered at runtime to re-compile programs into more efficient code. Such optimizations appear to be more easily incorporated within a semantics-directed compiler structure than within traditional compiler structure. We present a case study in which a metacomputation-based reference compiler for a small imperative language converts easily into a compiler which performs a particular profile-driven optimization: local register allocation. Our reference compiler is implemented in the staged, functional language MetaML and takes full advantage of the synergy between metacomputation-style language definitions and the staging constructs of MetaML. We believe that the approach to implementing profile-driven optimizations presented here suggests a useful, formal model for dynamically adaptable software.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="Harrison2001">32</a>]
</td>
<td class="bibtexitem">
William Harrison.
 <em>Modular Compilers and Their Correctness Proofs</em>.
 PhD thesis, University of Illinois at Urbana-Champaign, 2001.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/dissertation.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This thesis explores the construction and correctness of modular compilers. Modular compilation is a compiler construction technique allowing the construction of compilers for high-level programming languages from reusable compiler building blocks. Modular compilers are defined in terms of denotational semantics based on monads, monad transformers, and a new model of staged computation called metacomputations. A novel form of denotational specification called observational program specification and related proof techniques are developed to assist in modular compiler verification. It will be demonstrated that the modular compilation framework provides both a level of modularity in compiler proofs as well as a useful organizing principle for such proofs.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HarrisonKamin2000">33</a>]
</td>
<td class="bibtexitem">
William Harrison and Samuel Kamin.
 Metacomputation-based compiler architecture.
 In <em>5th International Conference on the Mathematics of Program
  Construction, Ponte de Lima, Portugal</em>, volume 1837 of <em>Lecture Notes in
  Computer Science</em>, pages 213--229. Springer-Verlag, 2000.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/mpc00.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This paper presents a modular and extensible style of language specification based on metacomputations. This style uses two monads to factor the static and dynamic parts of the specification, thereby staging the specification and achieving strong binding-time separation. Because metacomputations are defined in terms of monads, they can be constructed modularly and extensibly using monad transformers. Consequently, metacomputation-style specifications are modular and extensible. A number of language constructs are specified: expressions, control-flow, imperative features, block structure, and higher-order functions and recursive bindings. Because of the strong binding-time separation, metacomputation-style specification lends itself to semantics-directed compilation, which we demonstrate by creating a modular compiler for a block-structured imperative, while language.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="HarrisonKamin1998">34</a>]
</td>
<td class="bibtexitem">
William&nbsp;L. Harrison and Samuel&nbsp;N. Kamin.
 Modular compilers based on monad transformers.
 In <em>Proceedings of the 1998 International Conference on Computer
  Languages</em>, pages 122--131. IEEE Computer Society Press, 1998.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/iccl98.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
The monadic style of language specification has the
                 advantages of modularity and extensibility: it is
                 simple to add or change features in an interpreter to
                 re ect modifications in the source language. It has
                 proven difficult to extend the method to compilation.
                 We demonstrate that by introducing machine-like stores
                 (code and data) into the monadic semantics and then
                 partially evaluating the resulting semantic
                 expressions, we can achieve many of the same advantages
                 for a compiler as for an interpreter. A number of
                 language constructs and features are compiled:
                 expressions, CBV and CBN evaluation of
                 &lambda;-expressions, dynamic scoping, and various
                 imperative features. The treatment of recursive
                 procedures is outlined as well. The resulting method
                 allows compilers to be constructed in a mix-and-match
                 fashion just as in a monad-structured interpreter.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="Harrison1992">35</a>]
</td>
<td class="bibtexitem">
William Harrison.
 Mechanizing the axiomatic semantics for a programming language with
  asynchronous send and receive in hol.
 Master's thesis, University of California, Davis, 1992.
[&nbsp;<a href="https://harrisonwl.github.io/assets/papers/masters-thesis.pdf">.pdf</a>&nbsp;]
<blockquote><font size="-1">
This thesis presents the axiomatic semantics for a simple distributed language and its mechanization in HOL. The constructs of this language include asynchronous send and synchronous receive statements as well as those basic to a sequential programming language. The language has the appearance of a system programming language that supports sequential execution extended with message-passing, and would be a suitable basis for coding distributed operating systems. Included in the mechanization are functions which generate the goals associated with the verification of simple statements in the language.
</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="harrison92">36</a>]
</td>
<td class="bibtexitem">
William Harrison, Karl Levitt, and Myla Archer.
 An hol mechanization of the axiomatic semantics of a simple
  distributed programming language.
 In <em>Proceedings of the International Workshop on Higher-Order
  Logic Theorem Proving and Its Applications</em>, pages 347--358, Leuven, Belgium,
  September 1992.
<blockquote><font size="-1">

</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="MeSeHOL">37</a>]
</td>
<td class="bibtexitem">
William Harrison and Karl Levitt.
 Mechanizing security in HOL.
 In <em>Proceedings of the 1991 International Workshop on the HOL
  Theorem Proving System and its Applications</em>, pages 63--66, Davis,
  California, 1991. IEEE Computer Society Press.
<blockquote><font size="-1">

</font></blockquote>
<p>
</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="tr92a">38</a>]
</td>
<td class="bibtexitem">
William Harrison, Karl Levitt, and Myla Archer.
 Towards a Verified Code Basis for a Secure Distributed Operating
  System.
 Technical Report CSE-92-19, University of California at Davis, 1992.

</td>
</tr>


<tr valign="top">
<td align="right" class="bibtexnumber">
[<a name="tr92b">39</a>]
</td>
<td class="bibtexitem">
William Harrison.
 Mechanizing the Axiomatic Semantics for a Programming Language with
  Asynchronous Send and Receive in HOL.
 Technical Report CSE-92-20, University of California at Davis, 1992.

</td>
</tr>
</table><hr>
</body>



