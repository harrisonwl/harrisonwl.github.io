---
layout: default
title: "Home"
---

{% comment %} Here is the syntax for figures from jekyll_figure.
{% figure 4Ps pdf 'Your caption here' %}
{% endcomment %}

<div class="main_block">

    <div class="inner_block">
    <img src="{{ site.baseurl }}/images/headshot-small.jpg"  style="display: block; margin: 0 auto; max-width: 50%;" alt="Screenshot" height="165" width="165"/>
    </div>

    <div class="inner_block">

    </div>    
</div>

<div></div>


#### __Biographical information__

* Currently, Senior Formal Methods Researcher/Directorate Fellow at Idaho National Laboratory
* Formerly, Senior Principal Research Scientist at Two Six Technologies
* Formerly, Senior Cybersecurity Research Scientist at Oak Ridge National Laboratory
* Formerly, Associate Professor of Computer Science at the University of Missouri 
* Recipient: NSF CAREER Award 2008 (CyberTrust program) 
* Post-doctoral Researcher at Oregon Graduate Institute from June 2000-August 2003 
* Visiting Lecturer (Computer Science), Indiana University, September 1999-May 2000 
* Ph.D (Computer Science), University of Illinois at Urbana-Champaign 2001 
* M.S. (Computer Science), University of California at Davis, August, 1992 
* B.A. (Mathematics), University of California at Berkeley, May 1986



#### __Research Interests__
Language-based methods in computer security, all aspects of programming languages design and implementation, functional high-level synthesis, Computer and information security, malware analysis, and formal methods.

#### __Contact Information__
* __Email:__ william.lawrence.harrison@gmail.com
* <a href = "https://harrisonwl.github.io/assets/cv/harrison-cv2024.pdf">Curriculum Vitae</a>

#### __External Links__
* <a href="https://www.researchgate.net/profile/William_Harrison6">My ResearchGate profile</a>
* <a href="https://github.com/twosixlabs/ReWire">The ReWire Functional Hardware Description Language.</a> ReWire is freely available and its most up-to-date toolchain is hosted by Two Six Technologies.
* <a href="https://my.berkeley.edu/profile/williamharrison5/">Berkeley Alumni Page</a>
* <a href="https://github.com/harrisonwl">My GitHub profile</a>

#### __Recent Publications__

* _Temporal Staging for Correct-by-Construction Cryptographic Hardware._, Yakir Forman and Bill Harrison. Proceedings of the 2024 Rapid Systems Prototyping (RSP24). [pdf]({{ site.baseurl }}/assets/papers/rsp24.pdf)
* _Formalized High Level Synthesis with Applications to Cryptographic Hardware._, Bill Harrison, Ian Blumenfeld, Eric Bond, Chris Hathhorn, Paul Li, May Torrence, and Jared Ziegler. Proceedings of the 2023 NASA Formal Methods Symposium (NFM23). [pdf]({{ site.baseurl }}/assets/papers/nfm23.pdf)
* _A Mechanized Semantic Metalanguage for High Level Synthesis._, Bill Harrison, Chris Hathhorn, and Gerard Allwein. Proceedings of PPDP 2021: 23rd International Symposium on Principles and Practice of Declarative Programming. [pdf]({{ site.baseurl }}/assets/papers/ppdp21.pdf) [code]({{ site.baseurl }}/assets/code/ppdp21codebase.tar.gz)
* _Strongly Bounded Termination with Applications to Security and Hardware Synthesis._, Tom Reynolds, Rohit Chadha, Bill Harrison, and Gerard Allwein. Proceedings of TyDe 2020: ACM Workshop on Type Driven Development. [pdf]({{ site.baseurl }}/assets/papers/tyde20.pdf)
* _Verifiable Security Templates for Hardware._, Bill Harrison and Gerard Allwein. Proceedings of 2020 Design, Automation, and Test Europe (DATE). [pdf]({{ site.baseurl }}/assets/papers/date20.pdf)
* _Language Abstractions for Hardware-based Control-Flow Integrity Monitoring._ Bill Harrison and Gerard Allwein. Proceedings of the 2018 International Conference on Reconfigurable Computing and FPGAs. [pdf]({{ site.baseurl }}/assets/papers/reconfig18.pdf)  [codebase]({{ site.baseurl }}/assets/code/ReConFig18codebase.tar.gz)
* _The Mechanized Marriage of Effects and Monads with Applications to High Assurance Hardware._ Tom Reynolds, Bill Harrison, Adam Procter, and Gerard Allwein. ACM Transactions on Embedded Computing Systems (Feb 2019). [pdf]({{ site.baseurl }}/assets/papers/tecs18.pdf)
* _Semantics-directed Prototyping of Hardware Runtime Monitors._ Bill Harrison and Gerard Allwein. Proceedings of the 29th International Symposium on Rapid System Prototyping (RSP). [pdf]({{ site.baseurl }}/assets/papers/rsp18.pdf)
* _A Core Calculus for Secure Hardware: Its Formal Semantics and Proof System._ Tom Reynolds, Adam Procter, Bill Harrison, and Gerard Allwein. 15th ACM-IEEE International Conference on Formal Methods and Models for System Design (MEMOCODE17). [pdf]({{ site.baseurl }}/assets/papers/memocode17.pdf)
* _A Programming Model for Reconfigurable Computing Based in Functional Concurrency_, William L. Harrison, Adam Procter, Ian Graves, Michela Becchi, and Gerard Allwein. ReCoSoC 2016 [pdf]({{ site.baseurl }}/assets/papers/recosoc16.pdf) [slides]({{ site.baseurl }}/assets/papers/slides-recosoc16.pdf).
* _Provably Correct Development of Reconfigurable Hardware Designs via Equational Reasoning_, Ian Graves, Adam Procter, William L. Harrison, and Gerard Allwein. FPT 2015 [pdf]({{ site.baseurl }}/assets/papers/fpt15.pdf) [slides]({{ site.baseurl }}/assets/papers/slides-fpt15.pdf).
* _A Principled Approach to Secure Multi-Core Processor Design with ReWire_, Adam Procter, William Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. ACM Transactions on Embedded Computing Systems [pdf]({{ site.baseurl }}/assets/papers/tecs16.pdf).
* _Semantics Driven Hardware Design, Implementation, and Verification with ReWire_, Adam Procter, William Harrison, Ian Graves, Michela Becchi, and Gerard Allwein. LCTES 2015 [pdf]({{ site.baseurl }}/assets/papers/lctes15.pdf).
* _Hardware Synthesis from Functional Embedded Domain-Specific Languages:
A Case Study in Regular Expression Compilation_, Ian Graves, Adam Procter, William Harrison, Michela Becchi, and Gerard Allwein. ARC 2015 [pdf]({{ site.baseurl }}/assets/papers/arc15.pdf).



{% comment %}
### Get Started

Start by [creating a new post](http://jekyllrb.com/docs/posts/) one of the categories listed in `_config.yml`. It will appear in the navigation on the left once recompiled. Or use the supplied script to make creating pages easier:

```bash
ruby bin/jekyll-page "Some Page Title" ref
```

#### Don't Forget

- Add your own content to this page (i.e. `index.md`) and change the `title`
- Change `title` and `subtitle` defined in `config.yml` for your site
- Set the `baseurl` in `_config.yml` for your repo if deploying to GitHub pages
{% endcomment %}
